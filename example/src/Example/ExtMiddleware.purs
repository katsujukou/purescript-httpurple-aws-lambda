module Example.ExtMiddleware where

import Prelude

import Example.Basic (endpoints)
import HTTPurple (notFound, usingCont)
import HTTPurple as HTTPurple
import HTTPurple.AWS.Lambda (APIGatewayV2, LambdaHandler, lambdaRouter, mkHandler)
import Prim.Row as Row
import Record as Record

authenticator
  :: forall route extIn extOut
   . Row.Union extIn (authUser :: { name :: String }) extOut
  => Row.Nub (HTTPurple.RequestR route extOut) (HTTPurple.RequestR route extOut)
  => HTTPurple.Middleware route extIn extOut
authenticator router req = router (Record.merge req { authUser: { name: "John" } })

handler :: LambdaHandler APIGatewayV2
handler = mkHandler
  { route: endpoints
  , router: lambdaRouter $ authenticator $ router
  }
  where
  router { method, route } = usingCont case route, method of

    _, _ -> notFound