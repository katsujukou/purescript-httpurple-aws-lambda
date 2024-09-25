module HTTPurple.AWS.Lambda.Handler
  ( LambdaHandler
  , mkHandler
  , mkHandlerWithStreaming
  ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Profunctor.Choice ((|||))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff, catchError, joinFiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception as Exn
import Effect.Uncurried (EffectFn2, EffectFn3, mkEffectFn2, mkEffectFn3)
import Foreign.Object as Object
import HTTPurple as HTTPurple
import HTTPurple.AWS.Lambda.Context (LambdaContext, useLambdaInputs)
import HTTPurple.AWS.Lambda.Request (LambdaExtRequest, mkLambdaExtRequest)
import HTTPurple.AWS.Lambda.Streaming (ResponseStream, toServerResponse, withMetadata)
import HTTPurple.AWS.Lambda.Trigger (class LambdaTrigger, TriggerType, toRequest, toResponse)
import HTTPurple.Headers as HTTPuepleHeaders
import Routing.Duplex as RD
import Unsafe.Coerce (unsafeCoerce)

type LambdaRoutingSettingsR trigger route output r =
  ( route :: RD.RouteDuplex' route
  , router :: LambdaExtRequest trigger route output -> HTTPurple.ResponseM
  | r
  )

type BasicLambdaRoutingSettings trigger route = { | LambdaRoutingSettingsR trigger route () () }

-- | The type of Lambda handler function.
type HandlerType event result = EffectFn2 event LambdaContext (Promise result)

type StreamifiedHandlerType event result =
  EffectFn3 event ResponseStream LambdaContext (Promise result)

foreign import data LambdaHandler :: TriggerType -> Type

asLambdaHandler
  :: forall trigger event result
   . HandlerType event result
  -> LambdaHandler trigger
asLambdaHandler = unsafeCoerce

foreign import asStreamingEnabledLambdaHandler
  :: forall trigger event result
   . StreamifiedHandlerType event result
  -> LambdaHandler trigger

mkHandlerInternal
  :: forall @trigger event result route
   . LambdaTrigger trigger event result
  => BasicLambdaRoutingSettings trigger route
  -> Maybe ResponseStream
  -> event
  -> LambdaContext
  -> Effect (Promise (Maybe result))
mkHandlerInternal op@{ route } mbResp event ctx = do
  launchAff handleRequest >>= joinFiber >>> fromAff
  where
  handleRequest = do
    let
      router = useLambdaInputs { event, ctx } $
        op.router <<< mkLambdaExtRequest

    httpurpleReq <- liftEffect $ toRequest @trigger route event
    httpurpleResp <- (onNotFound ||| handleInternelError router) httpurpleReq
    case mbResp of
      Just resp -> send resp httpurpleResp $> Nothing
      Nothing -> do
        resp <- toResponse @trigger httpurpleResp
        pure (Just resp)

  send :: ResponseStream -> HTTPurple.Response -> Aff Unit
  send respS' httpurpleResp = do
    respS <- liftEffect do
      let
        HTTPuepleHeaders.ResponseHeaders headers' = httpurpleResp.headers
        headers = headers' #
          foldlWithIndex (\k obj v -> Object.insert (unwrap k) (joinWith ";" v) obj) Object.empty
      withMetadata respS'
        { statusCode: httpurpleResp.status
        , headers
        }

    httpurpleResp.writeBody (toServerResponse respS)

  onNotFound = const HTTPurple.notFound

  handleInternelError router' req = do
    catchError (router' req) \err -> do
      liftEffect $ do
        Console.error $ Exn.message err
        Console.logShow $ Exn.stack err
      HTTPurple.internalServerError "Internal server error"

mkHandler
  :: forall @trigger event result route
   . LambdaTrigger trigger event result
  => BasicLambdaRoutingSettings trigger route
  -> LambdaHandler trigger
mkHandler op = asLambdaHandler $ mkEffectFn2 $ mkHandlerInternal op Nothing

mkHandlerWithStreaming
  :: forall @trigger event result route
   . LambdaTrigger trigger event result
  => BasicLambdaRoutingSettings trigger route
  -> LambdaHandler trigger
mkHandlerWithStreaming op = asStreamingEnabledLambdaHandler $
  mkEffectFn3 \evt stream -> mkHandlerInternal op (Just stream) evt