module HTTPurple.AWS.Lambda.Handler
  ( LambdaHandler
  , mkHandler
  -- , mkHandlerNodeMiddleware
  , mkHandlerWithStreaming
  ) where

import Prelude

import Control.Promise (Promise, fromAff)
import Data.Bifunctor (rmap)
import Data.Either (Either)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toNullable)
import Data.Profunctor (lcmap)
import Data.Profunctor.Choice ((|||))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff, catchError, joinFiber, launchAff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Effect.Exception as Exn
import Effect.Uncurried (EffectFn2, EffectFn3, mkEffectFn2, mkEffectFn3)
import Foreign.Object as Object
import HTTPurple (MiddlewareSettingsR)
import HTTPurple as HTTPurple
import HTTPurple.AWS.Lambda.Context (LambdaContext)
import HTTPurple.AWS.Lambda.Request (LambdaExtEventRequestNT(..), LambdaExtRequest, LambdaRequestR, mkLambdaExtRequest)
import HTTPurple.AWS.Lambda.Streaming (ResponseStream, toServerResponse, withMetadata)
import HTTPurple.AWS.Lambda.Trigger (class LambdaTrigger, TriggerType, toRequest, toResponse)
import HTTPurple.Headers (ResponseHeaders)
import HTTPurple.Headers as HTTPuepleHeaders
import Node.HTTP.Types (ServerResponse)
import Prim.Row as Row
import Prim.RowList (class RowToList)
import Record (merge)
import Record.Studio.Keys (class KeysRL)
import Routing.Duplex as RD
import Type.Row (type (+))
import Unsafe.Coerce (unsafeCoerce)

type LambdaRoutingSettingsR trigger route output r =
  ( route :: RD.RouteDuplex' route
  , router :: LambdaExtRequest trigger route output -> HTTPurple.ResponseM
  | r
  )

type BasicLambdaRoutingSettings trigger route = { | LambdaRoutingSettingsR trigger route () () }

type ExtLambdaRoutingSettings trigger route input output = { | LambdaRoutingSettingsR trigger route output + MiddlewareSettingsR input output }

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

fromLambdaRequest
  :: forall @trigger event result route ext thru extRL
   . LambdaTrigger trigger event result
  => Row.Union ext thru ext
  => RowToList ext extRL
  => Row.Nub (LambdaRequestR trigger event route ext) (LambdaRequestR trigger event route ext)
  => RD.RouteDuplex' route
  -> event
  -> LambdaContext
  -> Effect (Either (HTTPurple.Request Unit) (LambdaExtEventRequestNT trigger event route ext))
fromLambdaRequest route event ctx = do
  req <- liftEffect $ toRequest @trigger route event
  pure $ rmap (assumeExtention <<< (flip merge { lambdaInputs: { event, ctx } })) req
  where
  assumeExtention
    :: { | LambdaRequestR _ _ _ () }
    -> LambdaExtEventRequestNT trigger event route ext
  assumeExtention = unsafeCoerce >>> LambdaExtEventRequestNT

-- handleLambdaRequestWithMiddlware
--   :: forall trigger event result route ctx thru ctxRL input output
--    . LambdaTrigger trigger event result
--   => Row.Union ctx thru ctx
--   => Row.Nub (LambdaRequestR trigger event route ctx) (LambdaRequestR trigger event route ctx)
--   => RowToList ctx ctxRL
--   => Maybe ResponseStream
--   -> { route :: RD.RouteDuplex' route
--      , router :: LambdaExtEventRequestNT trigger event route ctx -> _
--      , nodeMiddleware :: NodeMiddlewareStack input output
--      }
--   -> event
--   -> LambdaContext
--   -> Aff (Maybe result)
-- handleLambdaRequestWithMiddlware mbResp { route, router, nodeMiddleware: NodeMiddlewareStack middleware } event ctx = do
--   eff <- liftEffect $ flip runContT (coerce >>> pure) $ middleware (MiddlewareResult { request: req, response: resp, middlewareResult: NotCalled })
--   executeHandler eff
--   where

--   executeHandler :: Partial => MiddlewareResult output -> m Unit
--   -- executeHandler (MiddlewareResult { request, response, middlewareResult: ProcessingFailed error }) =
--   --   handleRequestUnit (defaultMiddlewareErrorHandler error) request response
--   executeHandler (MiddlewareResult { request, response, middlewareResult: ProcessingSucceeded }) =
--     handleLambdaRequest mbResp { route, router } request response

-- executeHandler (MiddlewareResult { middlewareResult: NotCalled }) =
--   pure unit

handleLambdaRequest
  :: forall trigger event result route ctx thru ctxRL
   . LambdaTrigger trigger event result
  => Row.Union ctx thru ctx
  => Row.Nub (LambdaRequestR trigger event route ctx) (LambdaRequestR trigger event route ctx)
  => RowToList ctx ctxRL
  => Maybe ResponseStream
  -> { route :: RD.RouteDuplex' route
     , router ::
         LambdaExtEventRequestNT trigger event route ctx
         -> Aff
              { headers :: ResponseHeaders
              , status :: Int
              , writeBody :: ServerResponse -> Aff Unit
              }

     }
  -> event
  -> LambdaContext
  -> Aff (Maybe result)
handleLambdaRequest mbResp op@{ route } event ctx = do
  httpurpleReq <- liftEffect $ fromLambdaRequest route event ctx
  httpurpleResp <- (onNotFound ||| handleInternelError op.router) httpurpleReq
  case mbResp of
    Just resp -> send resp httpurpleResp $> Nothing
    Nothing -> do
      resp <- toResponse @trigger httpurpleResp
      pure (Just resp)
  where
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

mkHandlerInternal
  :: forall @trigger event result route output outputRL thru
   . LambdaTrigger trigger event result
  => Row.Union output thru output
  => RowToList output outputRL
  => KeysRL outputRL
  => Row.Nub (LambdaRequestR trigger event route output) (LambdaRequestR trigger event route output)
  => Maybe ResponseStream
  -- -> Maybe (NodeMiddlewareStack input output)
  -> { route :: RD.RouteDuplex' route
     , router :: LambdaExtEventRequestNT trigger event route output -> Aff HTTPurple.Response
     }
  -> event
  -> LambdaContext
  -> Effect (Promise (Nullable result))
mkHandlerInternal mbResp op event ctx = do
  launchAff handleRequest >>= joinFiber >>> map toNullable >>> fromAff
  where
  handleRequest = handleLambdaRequest mbResp op event ctx

-- Just nodeMiddleware -> handleRequest' mbResp (merge op { nodeMiddleware })

asExtended
  :: forall trigger event route ext
   . (LambdaExtRequest trigger route ext -> HTTPurple.ResponseM)
  -> LambdaExtEventRequestNT trigger event route ext
  -> HTTPurple.ResponseM
asExtended = lcmap (mkLambdaExtRequest <<< unwrap)

mkHandler
  :: forall @trigger event result route
   . LambdaTrigger trigger event result
  => BasicLambdaRoutingSettings trigger route
  -> LambdaHandler trigger
mkHandler op = asLambdaHandler $ mkEffectFn2 $
  mkHandlerInternal Nothing
    { route: op.route
    , router: asExtended op.router
    }

-- mkHandlerNodeMiddleware
--   :: forall @trigger event result route input output thru outputRL
--    . LambdaTrigger trigger event result
--   => Row.Union output thru output
--   => RowToList output outputRL
--   => KeysRL outputRL
--   => Row.Nub (LambdaRequestR trigger event route output) (LambdaRequestR trigger event route output)
--   => ExtLambdaRoutingSettings trigger route input output
--   -> LambdaHandler trigger
-- mkHandlerNodeMiddleware { route, router, nodeMiddleware } =
--   asLambdaHandler $ mkEffectFn2 $
--     mkHandlerInternal Nothing (Just nodeMiddleware)
--       { route
--       , router: asExtended router
--       }

mkHandlerWithStreaming
  :: forall @trigger event result route
   . LambdaTrigger trigger event result
  => BasicLambdaRoutingSettings trigger route
  -> LambdaHandler trigger
mkHandlerWithStreaming { route, router } = asStreamingEnabledLambdaHandler $
  mkEffectFn3 \evt stream -> mkHandlerInternal
    (Just stream)
    -- Nothing
    { route
    , router: asExtended router
    }
    evt
