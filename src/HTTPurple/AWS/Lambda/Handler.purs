module HTTPurple.AWS.Lambda.Handler
  ( LambdaHandler
  , mkHandlerWithStreaming
  ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.FoldableWithIndex (forWithIndex_)
import Data.Newtype (unwrap)
import Data.Profunctor.Choice ((|||))
import Data.String (joinWith)
import Effect.Aff (Aff, catchError, joinFiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception as Exn
import Effect.Uncurried (EffectFn2, EffectFn3, mkEffectFn3)
import HTTPurple as HTTPurple
import HTTPurple.AWS.Lambda.Context (LambdaContext, useLambdaInputs)
import HTTPurple.AWS.Lambda.Request (LambdaExtRequest, mkLambdaExtRequest)
import HTTPurple.AWS.Lambda.Streaming (ResponseStream, setHeader, setStatusCode, toServerResponse)
import HTTPurple.AWS.Lambda.Trigger (class LambdaTrigger, TriggerType, toRequest)
import HTTPurple.Headers as HTTPuepleHeaders
import Routing.Duplex as RD

type LambdaRoutingSettingsR trigger route output r =
  ( route :: RD.RouteDuplex' route
  , router :: LambdaExtRequest trigger route output -> HTTPurple.ResponseM
  | r
  )

type BasicLambdaRoutingSettings trigger route = { | LambdaRoutingSettingsR trigger route () () }

-- | The type of Lambda handler function.
type HandlerType event result = EffectFn2 event LambdaContext (Promise result)

type StreamifiedHandlerType event =
  EffectFn3 event ResponseStream LambdaContext (Promise Unit)

foreign import data LambdaHandler :: TriggerType -> Type

-- asLambdaHandler
--   :: forall trigger event result
--    . HandlerType event result
--   -> LambdaHandler trigger
-- asLambdaHandler = unsafeCoerce

foreign import asStreamingEnabledLambdaHandler
  :: forall trigger event
   . StreamifiedHandlerType event
  -> LambdaHandler trigger

mkHandlerWithStreaming
  :: forall @trigger event result route
   . LambdaTrigger trigger event result
  => BasicLambdaRoutingSettings trigger route
  -> LambdaHandler trigger
mkHandlerWithStreaming op@{ route } = asStreamingEnabledLambdaHandler $
  mkEffectFn3 \evt resp ctx -> do
    fib <- launchAff $ handleRequest ctx resp evt
    fromAff $ joinFiber fib
  where
  handleRequest :: LambdaContext -> ResponseStream -> event -> Aff _
  handleRequest ctx resp event = do
    let
      router = useLambdaInputs { event, ctx } $
        op.router <<< mkLambdaExtRequest

    httpurpleReq <- liftEffect $ toRequest @trigger route event
    httpurpleResp <- (onNotFound ||| handleInternelError router) httpurpleReq
    send resp httpurpleResp

  -- resp <- toResponse @trigger httpurpleResp
  -- pure resp

  send :: ResponseStream -> HTTPurple.Response -> Aff Unit
  send respS httpurpleResp = do
    liftEffect do
      respS # setStatusCode httpurpleResp.status
      case httpurpleResp.headers of
        HTTPuepleHeaders.ResponseHeaders headers -> do
          forWithIndex_ headers \k v -> do
            respS # setHeader (unwrap k) (joinWith ";" v)
    httpurpleResp.writeBody (toServerResponse respS)

  onNotFound = const HTTPurple.notFound

  handleInternelError router' req = do
    catchError (router' req) \err -> do
      liftEffect $ do
        Console.error $ Exn.message err
        Console.logShow $ Exn.stack err
      HTTPurple.internalServerError "Internal server error"
