module HTTPurple.AWS.Lambda.Trigger
  ( TriggerType
  , APIGateway
  , APIGatewayV2
  , ALB
  , class LambdaTrigger
  , toRequest
  , toResponse
  ) where

import Prelude

import Data.Bitraversable (bitraverse)
import Data.Either (Either)
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import HTTPurple (Request, RouteDuplex', Response)
import HTTPurple.AWS.Lambda.Response (extractBody, foldHeaders)
import HTTPurple.AWS.Lambda.Trigger.APIGatewayV2 (APIGatewayProxyEventV2, APIGatewayProxyResultV2)
import HTTPurple.Headers as Headers
import HTTPurple.Method as Method
import HTTPurple.Path as Path
import HTTPurple.Query as Query
import HTTPurple.Version as Version
import Node.Buffer.Class as Buffer
import Node.Encoding (Encoding(..))
import Node.HTTP.IncomingMessage as IM
import Node.HTTP.Types (IMServer, IncomingMessage)
import Node.Stream (readableFromBuffer)
import Routing.Duplex as RD
import Unsafe.Coerce (unsafeCoerce)

data TriggerType

foreign import data APIGateway :: TriggerType
foreign import data APIGatewayV2 :: TriggerType
foreign import data ALB :: TriggerType

class LambdaTrigger :: forall k. k -> Type -> Type -> Constraint
class LambdaTrigger trigger event result | trigger -> event result where
  toRequest
    :: forall route
     . RouteDuplex' route
    -> event
    -> Effect (Either (Request Unit) (Request route))
  toResponse :: Response -> Aff result

instance apiGatewayV2LambdaTrigger ::
  LambdaTrigger
    APIGatewayV2
    APIGatewayProxyEventV2
    APIGatewayProxyResultV2
  where
  toRequest routes evt = do
    let
      url = evt.rawPath <> "?" <> evt.rawQueryString

      request = unsafeCoerce
        { url
        , headers: evt.headers
        , method: evt.requestContext.http.method
        , httpVersion: Str.replace (Pattern "HTTP/") (Replacement "") evt.requestContext.http.protocol
        }

    RD.parse routes url #
      bitraverse (const $ mkRequest request unit) (mkRequest request)
    where
    mkRequest :: forall route. IncomingMessage IMServer -> route -> Effect (Request route)
    mkRequest req route = do
      body <- liftEffect do
        buffer <- Ref.new Nothing
        string <- Ref.new Nothing
        bodyBuf <- Buffer.fromString evt.body $ if evt.isBase64Encoded then Base64 else UTF8
        stream <- readableFromBuffer bodyBuf
        pure { buffer, stream, string }

      pure
        { method: Method.read req
        , path: Path.read req
        , query: Query.read req
        , route: route
        , headers: Headers.read req
        , body
        , httpVersion: Version.read req
        , url: IM.url req
        }

  toResponse httpurpleResp = do
    mbBody <- extractBody httpurpleResp
    pure
      { statusCode: httpurpleResp.status
      , body: toNullable mbBody
      , cookies: toNullable Nothing
      , headers: httpurpleResp.headers # foldHeaders # toNullable
      , isBase64Encoded: Nothing # toNullable
      }
