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

import Data.Array as Array
import Data.Bitraversable (bitraverse)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe, toNullable)
import Data.String (Pattern(..), Replacement(..))
import Data.String as Str
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Foreign.Object (Object)
import Foreign.Object as Object
import HTTPurple (Request, RouteDuplex', Response)
import HTTPurple.AWS.Lambda.Trigger.ALB (ALBEvent, ALBResult)
import HTTPurple.AWS.Lambda.Trigger.APIGateway (APIGatewayProxyEvent, APIGatewayProxyResult)
import HTTPurple.AWS.Lambda.Trigger.APIGatewayV2 (APIGatewayProxyEventV2, APIGatewayProxyResultV2)
import HTTPurple.Headers as Headers
import HTTPurple.Method as Method
import HTTPurple.Path as Path
import HTTPurple.Query as Query
import HTTPurple.Version as Version
import Node.Buffer.Class as Buffer
import Node.Encoding (Encoding(..))
import Node.EventEmitter as EE
import Node.HTTP.IncomingMessage as IM
import Node.HTTP.Types (IMServer, IncomingMessage)
import Node.Stream (dataH, endH, readableFromBuffer)
import Node.Stream as Stream
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
        let
          body' = toMaybe evt.body # fromMaybe ""
        bodyBuf <- Buffer.fromString body' $ if evt.isBase64Encoded then Base64 else UTF8
        stream <- readableFromBuffer bodyBuf
        pure { buffer, stream, string }

      pure $
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
    pure $
      { statusCode: httpurpleResp.status
      , body: toNullable mbBody
      , cookies: toNullable Nothing
      , headers: httpurpleResp.headers # foldHeaders # toNullable
      , isBase64Encoded: Nothing # toNullable
      }

else instance albLambdaTrigger ::
  LambdaTrigger ALB ALBEvent ALBResult
  where
  toRequest _ _ = unsafeCoerce {}
  toResponse = unsafeCoerce {}

else instance apiGatewayLambdaTrigger ::
  LambdaTrigger APIGateway APIGatewayProxyEvent APIGatewayProxyResult
  where
  toRequest _ _ = unsafeCoerce {}
  toResponse = unsafeCoerce {}

extractBody :: Response -> Aff (Maybe String)
extractBody { writeBody } = makeAff \done -> do
  launchAff_ $ writeBody (unsafeCoerce done)
  pure nonCanceler

foldHeaders :: Headers.ResponseHeaders -> Maybe (Object String)
foldHeaders (Headers.ResponseHeaders headers)
  | Map.isEmpty headers = Nothing
  | otherwise = headers
      # Map.toUnfoldable
      # Array.foldl (\prev (Tuple k v) -> Object.insert (unwrap k) (Str.joinWith ";" v) prev) Object.empty
      # Just