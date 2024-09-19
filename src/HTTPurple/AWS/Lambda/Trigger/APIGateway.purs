module HTTPurple.AWS.Lambda.Trigger.APIGateway where

import Prelude

import Data.Nullable (Nullable)
import Foreign.Object (Object)
import HTTPurple.AWS.Lambda.Trigger.Types (SomeAuthorizer)
import Record as Record
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

type APIGatewayProxyResult =
  { statusCode :: Int
  , headers :: Nullable (Object String)
  , multiValueHeaders :: Nullable (Object (Array String))
  , body :: String
  , isBase64Encoded :: Nullable Boolean
  }

type APIGatewayProxyEvent =
  { body :: Nullable String
  , headers :: Object (Nullable String)
  , multiValueHeaders :: Object (Nullable (Array String))
  , httpMethod :: String
  , isBase64Encoded :: Boolean
  , path :: String
  , pathParameters :: Nullable (Object (Nullable String))
  , queryStringParameters :: Nullable (Object (Nullable String))
  , multiValueQueryStringParameters :: Nullable (Object (Nullable (Array String)))
  , stageVariables :: Nullable (Object (Nullable String))
  , requestContext :: APIGatewayRequestContext
  , resource :: String
  }

type APIGatewayRequestContext =
  { accountId :: String
  , apiId :: String
  , authorizer :: SomeAuthorizer
  , connectedAt :: Nullable Number
  , connectionId :: Nullable String
  , domainName :: Nullable String
  , domainPrefix :: Nullable String
  , eventType :: Nullable String
  , extendedRequestId :: Nullable String
  , protocol :: String
  , httpMethod :: String
  , identity :: APIGatewayEventIdentity
  , messageDirection :: Nullable String
  , messageId :: Nullable String
  , path :: String
  , stage :: String
  , requestId :: String
  , requestTime :: Nullable String
  , requestTimeEpoch :: Number
  , resourceId :: String
  , resourcePath :: String
  , routeKey :: Nullable String
  }

type APIGatewayEventIdentity =
  { accessKey :: Nullable String
  , accountId :: Nullable String
  , apiKey :: Nullable String
  , apiKeyId :: Nullable String
  , caller :: Nullable String
  , clientCert :: Nullable APIGatewayEventClientCertificate
  , cognitoAuthenticationProvider :: Nullable String
  , cognitoAuthenticationType :: Nullable String
  , cognitoIdentityId :: Nullable String
  , cognitoIdentityPoolId :: Nullable String
  , principalOrgId :: Nullable String
  , sourceIp :: String
  , user :: Nullable String
  , userAgent :: Nullable String
  , userArn :: Nullable String
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

type APIGatewayProxyCognitoAuthorizer =
  { claims :: Object String
  }

type APIGatewayProxyLambdaAuthorizer =
  { principalId :: String
  , integrationLatency :: Number
  }

asCongnitoAuthorizerContext
  :: forall r
   . (APIGatewayProxyCognitoAuthorizer -> r)
  -> SomeAuthorizer
  -> r
asCongnitoAuthorizerContext = unsafeCoerce

asLambdaAuthorizerContext
  :: forall r
   . (Object (Nullable String) -> APIGatewayProxyLambdaAuthorizer -> r)
  -> SomeAuthorizer
  -> r
asLambdaAuthorizerContext k ctx =
  let
    (authorizerCtx :: APIGatewayProxyLambdaAuthorizer) = unsafeCoerce ctx

    lambdaAuthorizer :: APIGatewayProxyLambdaAuthorizer
    lambdaAuthorizer =
      { principalId: authorizerCtx.principalId
      , integrationLatency: authorizerCtx.integrationLatency
      }

    custom :: Object (Nullable String)
    custom = authorizerCtx
      # Record.delete (Proxy :: _ "principalId")
      # Record.delete (Proxy :: _ "integrationLatency")
      # unsafeCoerce

  in
    k custom lambdaAuthorizer