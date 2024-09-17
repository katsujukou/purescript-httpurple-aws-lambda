module HTTPurple.AWS.Lambda
  ( Handler
  , mkHandler
  )
  where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Profunctor.Choice ((|||))
import Effect.Aff (Aff, catchError, joinFiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception as Exn
import Effect.Uncurried (EffectFn2, mkEffectFn2)
import HTTPurple as HTTPurple
import HTTPurple.AWS.Lambda.Trigger (class LambdaTrigger, toRequest, toResponse)
import HTTPurple.AWS.Lambda.Context (LambdaContext)

-- | The type of Lambda handler function.
type Handler event result = EffectFn2 event LambdaContext (Promise result)

mkHandler :: forall @trigger event result route. 
  LambdaTrigger trigger event result => 
  HTTPurple.BasicRoutingSettings route ->
  Handler event result
mkHandler { route, router } = mkEffectFn2 \evt _ -> do 
  fib <- launchAff $ handleRequest evt
  fromAff $ joinFiber fib
  where
    handleRequest :: event -> Aff _
    handleRequest evt = do
      httpurpleReq <- liftEffect $ toRequest @trigger route evt
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