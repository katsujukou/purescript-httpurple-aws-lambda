module Main where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.Show.Generic (genericShow)
import Effect.Aff (Aff)
import HTTPurple (Method(..), notFound, ok)
import HTTPurple as HTTPurple
import HTTPurple.AWS.Lambda (Handler, mkHandler)
import HTTPurple.AWS.Lambda.Trigger (APIGatewayV2)
import HTTPurple.AWS.Lambda.Trigger.APIGatewayV2 (APIGatewayProxyEventV2, APIGatewayProxyResultV2)
import Routing.Duplex (optional, string)
import Routing.Duplex as RD
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((?))

data Route = Greet { name :: Maybe String }

derive instance Generic Route _
instance Show Route where
  show = genericShow

route :: RD.RouteDuplex' Route
route = RD.root $ RDG.sum
  { "Greet": "greet" ? { name: optional <<< string }
  }

type RequestBody = { ultimate_answer :: Int }

codec :: CA.JsonCodec RequestBody
codec = CA.object "RequestBody" $
  CAR.record
    { ultimate_answer: CA.int
    }

router :: HTTPurple.Request Route -> Aff HTTPurple.Response
router { method, route: endpoint, body } = HTTPurple.usingCont case method, endpoint of
  Get, Greet { name } -> do
    ok $ "Hello, " <> (fromMaybe "World" name) <> "!"

  Post, Greet _ -> do
    let
      decoder = HTTPurple.JsonDecoder $
        (jsonParser >>> lmap (const unit))
          >=> (CA.decode codec >>> lmap (const unit))
    data_ <- HTTPurple.fromJson decoder body
    ok $ "You gave me: " <> show data_.ultimate_answer

  _, _ -> notFound

handler :: Handler APIGatewayProxyEventV2 APIGatewayProxyResultV2
handler = mkHandler @APIGatewayV2 { route, router }
