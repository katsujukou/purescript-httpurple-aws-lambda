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
  , queryStringParameters :: Nullable (Object (Nullable String))
  , headers :: Nullable (Object (Nullable String))
  , multiValueQueryStringParameters :: Nullable (Object (Nullable (Array String)))
  , multiValueHeaders :: Nullable (Object (Nullable (Array String)))
  , body :: Nullable String
  , isBase64Encoded :: Boolean
  }

type ALBResult =
  { statusCode :: Int
  , statusDescription :: Nullable String
  , headers :: Nullable (Object String)
  , multiValueHeaders :: Nullable (Object (Array String))
  , body :: Nullable String
  , isBase64Encoded :: Nullable Boolean
  }