module HTTPurple.AWS.Lambda.Request
  ( LambdaExtEventRequestNT(..)
  , LambdaExtRequest
  , LambdaRequest
  , LambdaRequestR
  , lambdaRouter
  , mkLambdaExtRequest
  ) where

import Data.Newtype (class Newtype)
import HTTPurple as HTTPurple
import HTTPurple.AWS.Lambda.Context (LambdaInputs)
import HTTPurple.AWS.Lambda.Trigger (class LambdaTrigger, TriggerType)
import Unsafe.Coerce (unsafeCoerce)

type LambdaRequestR :: TriggerType -> Type -> Type -> Row Type -> Row Type
type LambdaRequestR trigger event route output = HTTPurple.RequestR route (lambdaInputs :: LambdaInputs event | output)

type LambdaExtEventRequest trigger event route output = { | LambdaRequestR trigger event route output }

newtype LambdaExtEventRequestNT trigger event route output =
  LambdaExtEventRequestNT { | LambdaRequestR trigger event route output }

derive instance newtypeLambdaExtEventRequest ::
  Newtype (LambdaExtEventRequestNT t e r o) _

type LambdaEventRequest trigger event route = LambdaExtEventRequest trigger event route ()

foreign import data LambdaExtRequest
  :: TriggerType
  -> Type
  -> Row Type
  -> Type

type LambdaRequest trigger route = LambdaExtRequest trigger route ()

mkLambdaExtRequest :: forall trigger event route output. LambdaExtEventRequest trigger event route output -> LambdaExtRequest trigger route output
mkLambdaExtRequest = unsafeCoerce

unLambdaExtRequest
  :: forall @trigger event route output
   . LambdaExtRequest trigger route output
  -> LambdaExtEventRequest trigger event route output
unLambdaExtRequest = unsafeCoerce

lambdaRouter
  :: forall @trigger m event result route output
   . LambdaTrigger trigger event result
  => (LambdaExtEventRequest trigger event route output -> m HTTPurple.Response)
  -> LambdaExtRequest trigger route output
  -> m HTTPurple.Response
lambdaRouter router req = router (unLambdaExtRequest req)