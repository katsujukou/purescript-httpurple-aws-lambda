module HTTPurple.AWS.Lambda.Trigger.ALB where

import Data.Nullable (Nullable)
import Foreign.Object (Object)

type ALBEvent =
  { requestContext ::
      { elb ::
          { targetGroupArn :: String }
      }
  , httpMethod :: String
  , path :: String
  , headers :: Object String
  , queryStringParameters :: Nullable (Object String)
  , body :: Nullable String
  , isBase64Encoded :: Boolean
  }

type ALBResult =
  { statusCode :: Int
  , statusDescription :: String
  , headers :: Object String
  , body :: String
  , isBase64Encoded :: Nullable Boolean
  }