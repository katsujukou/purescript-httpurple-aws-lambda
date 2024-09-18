module HTTPurple.AWS.Lambda.Request
  ( LambdaExtRequest
  -- , LambdaExtRequestE
  , LambdaRequest
  -- , LambdaRequestE
  , mkLambdaExtRequest
  , lambdaRouter
  ) where

import HTTPurple as HTTPurple
import HTTPurple.AWS.Lambda.Context (LambdaInputs)
import HTTPurple.AWS.Lambda.Trigger (class LambdaTrigger, TriggerType)
import Unsafe.Coerce (unsafeCoerce)

foreign import data LambdaExtRequest
  :: TriggerType
  -> Type
  -> Row Type
  -> Type

type LambdaRequest trigger route = LambdaExtRequest trigger route ()

type LambdaExtRequestE :: TriggerType -> Type -> Type -> Row Type -> Type
type LambdaExtRequestE trigger event route output = HTTPurple.ExtRequest route (lambdaInputs :: LambdaInputs event | output)

type LambdaRequestE trigger event route = LambdaExtRequestE trigger event route ()

mkLambdaExtRequest :: forall trigger event route output. LambdaExtRequestE trigger event route output -> LambdaExtRequest trigger route output
mkLambdaExtRequest = unsafeCoerce

unLambdaExtRequest
  :: forall @trigger event route output
   . LambdaExtRequest trigger route output
  -> LambdaExtRequestE trigger event route output
unLambdaExtRequest = unsafeCoerce

lambdaRouter
  :: forall @trigger event result route output
   . LambdaTrigger trigger event result
  => (LambdaExtRequestE trigger event route output -> HTTPurple.ResponseM)
  -> LambdaExtRequest trigger route output
  -> HTTPurple.ResponseM
lambdaRouter router req = router (unLambdaExtRequest req)