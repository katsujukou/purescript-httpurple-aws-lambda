module HTTPurple.AWS.Lambda.Trigger.APIGatewayV2
  ( APIGatewayEventClientCertificate
  , APIGatewayEventRequestContextV2
  , APIGatewayProxyEventV2
  , APIGatewayProxyResultV2
  ) where

import Data.Nullable (Nullable)
import Foreign.Object (Object)
import HTTPurple.AWS.Lambda.Trigger.Types (SomeAuthorizer)

type APIGatewayProxyEventV2 =
  { version :: String
  , routeKey :: String
  , rawPath :: String
  , rawQueryString :: String
  , cookies :: Nullable (Array String)
  , headers :: Object String
  , queryStringParameters :: Nullable (Object (Nullable String))
  , body :: Nullable String
  , requestContext :: APIGatewayEventRequestContextV2
  , pathParameters :: Nullable (Object (Nullable String))
  , stageVariables :: Nullable (Object (Nullable String))
  , isBase64Encoded :: Boolean
  }

type APIGatewayEventRequestContextV2 =
  { accountId :: String
  , apiId :: String
  , authentication :: Nullable APIGatewayEventClientCertificate
  , authorizer :: Nullable SomeAuthorizer
  , domainName :: String
  , domainPrefix :: String
  , http ::
      { method :: String
      , path :: String
      , protocol :: String
      , sourceIp :: String
      , userAgent :: String
      }
  , requestId :: String
  , routeKey :: String
  , stage :: String
  , time :: String
  , timeEpoch :: Number
  }

type APIGatewayEventClientCertificate =
  { clientCertPem :: String
  , serialNumber :: String
  , subjectDN :: String
  , issuerDN :: String
  , validity ::
      { notAfter :: String
      , notBefore :: String
      }
  }

type APIGatewayProxyResultV2 =
  { statusCode :: Int
  , body :: Nullable String
  , headers :: Nullable (Object String)
  , isBase64Encoded :: Nullable Boolean
  , cookies :: Nullable (Array String)
  }