module HTTPurple.AWS.Lambda.Context where

import Data.Nullable (Nullable)
import Effect (Effect)
import Foreign (Foreign)

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