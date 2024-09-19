module Example.ExtMiddleware where

-- import Prelude

-- import Data.JSDate (JSDate, toISOString)
-- import Data.JSDate as JSDate
-- import Data.Maybe (Maybe(..), fromMaybe)
-- import Effect.Class (liftEffect)
-- import Effect.Class.Console as Console
-- import Example.Basic (Route(..), route)
-- import HTTPurple (Method(..), Middleware, notFound, ok, unauthorized)
-- import HTTPurple as HTTPurple
-- import HTTPurple as RD
-- import HTTPurple.AWS.Lambda (Handler, mkHandler)
-- import HTTPurple.AWS.Lambda.Middleware (useLambdaInputs)
-- import HTTPurple.AWS.Lambda.Trigger (APIGatewayV2)
-- import HTTPurple.AWS.Lambda.Trigger.APIGatewayV2 (APIGatewayProxyEventV2, APIGatewayProxyResultV2)
-- import Prim.Row (class Nub, class Union)
-- import Record as Record

-- authenticator
--   :: forall route extIn extOut
--    . Nub (HTTPurple.RequestR route extOut) (HTTPurple.RequestR route extOut)
--   => Union extIn (user :: Maybe String) extOut
--   => Middleware route extIn extOut
-- authenticator router req = case HTTPurple.lookup req.headers "X-Token" of
--   Just token | token == "123" -> router $ Record.merge req { user: Just "John Doe" }
--   _ -> router $ Record.merge req { user: Nothing :: Maybe String }

-- requestTime
--   :: forall route extIn extOut
--    . Nub (HTTPurple.RequestR route extOut) (HTTPurple.RequestR route extOut)
--   => Union extIn (time :: JSDate) extOut
--   => Middleware route extIn extOut
-- requestTime router request = do
--   time <- liftEffect JSDate.now
--   router $ Record.merge request { time }

-- middlewareStack
--   :: forall route
--    . (HTTPurple.ExtRequest route (user :: Maybe String, time :: JSDate) -> HTTPurple.ResponseM)
--   -> HTTPurple.Request route
--   -> HTTPurple.ResponseM
-- middlewareStack = authenticator <<< requestTime <<< useLambdaInputs

-- handler :: Handler APIGatewayProxyEventV2 APIGatewayProxyResultV2
-- handler = mkHandler @APIGatewayV2
--   { route
--   , router: middlewareStack router
--   }
--   where
--   router
--     :: HTTPurple.ExtRequest Route (user :: Maybe String, time :: JSDate)
--     -> HTTPurple.ResponseM
--   router req@{ method, route: endpoint } = do
--     currentTime <- liftEffect $ toISOString req.time
--     Console.log $
--       "[INFO - " <> currentTime <> "] New request:" <> show method <> RD.print route endpoint
--     case method, endpoint of
--       Get, Greet { name } -> do
--         ok $ "Hello ," <> fromMaybe "World!" name <> "!"

--       Post, Greet _
--         | Just user <- req.user -> do
--             ok $ "You gave me answer"
--         | otherwise -> unauthorized

--       _, _ -> notFound