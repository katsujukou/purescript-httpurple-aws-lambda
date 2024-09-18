module HTTPurple.AWS.Lambda.Context where

import Prelude

import Data.Nullable (Nullable)
import Effect (Effect)
import Foreign (Foreign)
import HTTPurple as HTTPurple
import Prim.Row (class Nub, class Union)
import Record as Record

type LambdaInputs event =
  { ctx :: LambdaContext
  , event :: event
  }

type LambdaContext =
  { callbackWaitsForEmptyEventLoop :: Boolean
  , functionName :: String
  , functionVersion :: String
  , invokedFunctionArn :: String
  , memoryLimitInMB :: String
  , awsRequestId :: String
  , logGroupName :: String
  , logStreamName :: String
  , identity :: Nullable CognitoIdentity
  , clientContext :: Nullable ClientContext
  , getRemainingTimeInMillis :: Effect Number
  }

type CognitoIdentity =
  { cognitoIdentityId :: String
  , cognitoIdentityPoolId :: String
  }

type ClientContext =
  { client :: ClientContextClient
  , "Custom" :: Nullable Foreign
  , env :: ClientContextEnv
  }

type ClientContextClient =
  { installationId :: String
  , appTitle :: String
  , appVersionName :: String
  , appVersionCode :: String
  , appPackageName :: String
  }

type ClientContextEnv =
  { platformVersion :: String
  , platform :: String
  , make :: String
  , model :: String
  , locale :: String
  }

-- | Middleware which injects raw Lambda inputs (event, context) into request.
useLambdaInputs
  :: forall event m route extIn extOut
   . Nub (HTTPurple.RequestR route extOut) (HTTPurple.RequestR route extOut)
  => Union extIn (lambdaInputs :: LambdaInputs event) extOut
  => LambdaInputs event
  -> HTTPurple.MiddlewareM m route extIn extOut
useLambdaInputs lambdaInputs router request = router $
  Record.merge request { lambdaInputs }
