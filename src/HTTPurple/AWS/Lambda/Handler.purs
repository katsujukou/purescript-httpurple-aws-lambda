module HTTPurple.AWS.Lambda.Handler
  ( LambdaHandler
  , mkHandler
  ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Profunctor.Choice ((|||))
import Effect.Aff (Aff, catchError, joinFiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception as Exn
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import HTTPurple as HTTPurple
import HTTPurple as RD
import HTTPurple.AWS.Lambda.Context (LambdaContext, useLambdaInputs)
import HTTPurple.AWS.Lambda.Request (LambdaExtRequest, mkLambdaExtRequest)
import HTTPurple.AWS.Lambda.Trigger (class LambdaTrigger, TriggerType, toRequest, toResponse)
import Unsafe.Coerce (unsafeCoerce)

type LambdaRoutingSettingsR trigger route output r =
  ( route :: RD.RouteDuplex' route
  , router :: LambdaExtRequest trigger route output -> HTTPurple.ResponseM
  | r
  )

type BasicLambdaRoutingSettings trigger route = { | LambdaRoutingSettingsR trigger route () () }

-- | The type of Lambda handler function.
type HandlerType event result = EffectFn2 event LambdaContext (Promise result)

foreign import data LambdaHandler :: TriggerType -> Type

asLambdaHandler
  :: forall trigger event result
   . HandlerType event result
  -> LambdaHandler trigger
asLambdaHandler = unsafeCoerce

mkHandler
  :: forall @trigger event result route
   . LambdaTrigger trigger event result
  => BasicLambdaRoutingSettings trigger route
  -> LambdaHandler trigger
mkHandler op@{ route } = asLambdaHandler $
  mkEffectFn2 \evt ctx -> do
    fib <- launchAff $ handleRequest ctx evt
    fromAff $ joinFiber fib
  where
  handleRequest :: LambdaContext -> event -> Aff _
  handleRequest ctx event = do
    let
      router = useLambdaInputs { event, ctx } $
        op.router <<< mkLambdaExtRequest

    httpurpleReq <- liftEffect $ toRequest @trigger route event
    httpurpleResp <- (onNotFound ||| handleInternelError router) httpurpleReq
    resp <- toResponse @trigger httpurpleResp
    pure resp

  onNotFound = const HTTPurple.notFound

  handleInternelError router' req = do
    catchError (router' req) \err -> do
      liftEffect $ do
        Console.error $ Exn.message err
        Console.logShow $ Exn.stack err
      HTTPurple.internalServerError "Internal server error"
