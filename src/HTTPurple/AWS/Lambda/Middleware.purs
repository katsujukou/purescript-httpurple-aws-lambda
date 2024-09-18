module HTTPurple.AWS.Lambda.Middleware where

import Prelude

import HTTPurple (MiddlewareM, RequestR)
import HTTPurple.AWS.Lambda.Context (LambdaContext)
import HTTPurple.AWS.Lambda.Trigger (class LambdaTrigger)
import Prim.Row (class Nub, class Union)
import Record (merge)

-- type LambdaInputsR event =
--   { ctx :: LambdaContext
--   , event :: event
--   }
