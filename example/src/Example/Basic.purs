module Example.Basic where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.Show.Generic (genericShow)
import Effect.Class.Console as Console
import HTTPurple (Method(..), notFound, ok)
import HTTPurple as HTTPurple
import HTTPurple.AWS.Lambda (APIGatewayV2, LambdaHandler, lambdaRouter, mkHandler)
import Routing.Duplex (optional, string)
import Routing.Duplex as RD
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((?))

data Endpoint = Greet { name :: Maybe String }

derive instance Generic Endpoint _
instance Show Endpoint where
  show = genericShow

endpoints :: RD.RouteDuplex' Endpoint
endpoints = RD.root $ RD.prefix "basic" $ RDG.sum
  { "Greet": "greet" ? { name: optional <<< string }
  }

type RequestBody = { ultimate_answer :: Int }

codec :: CA.JsonCodec RequestBody
codec = CA.object "RequestBody" $
  CAR.record
    { ultimate_answer: CA.int
    }

handler :: LambdaHandler APIGatewayV2
handler = mkHandler { route: endpoints, router }
  where
  router = lambdaRouter \req@{ method, route, lambdaInputs } -> HTTPurple.usingCont $
    case method, route of
      Get, Greet { name } -> do
        Console.logShow lambdaInputs.event.requestContext.http
        ok $ "\"Hello, " <> (fromMaybe "World" name) <> "!\""

      Post, Greet _ -> do
        let
          decoder = HTTPurple.JsonDecoder $
            (jsonParser >>> lmap (const unit))
              >=> (CA.decode codec >>> lmap (const unit))
        data_ <- HTTPurple.fromJson decoder req.body
        ok $ "You gave me: " <> show data_.ultimate_answer

      _, _ -> notFound
