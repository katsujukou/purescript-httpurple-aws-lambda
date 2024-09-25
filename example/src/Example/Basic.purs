module Example.Basic where

import Prelude

import Data.Argonaut.Parser (jsonParser)
import Data.Bifunctor (lmap)
import Data.Codec.Argonaut as CA
import Data.Codec.Argonaut.Record as CAR
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe, fromMaybe)
import Data.Show.Generic (genericShow)
import HTTPurple (Method(..), notFound, ok)
import HTTPurple as HTTPurple
import HTTPurple.AWS.Lambda (APIGatewayV2, LambdaHandler, lambdaRouter)
import HTTPurple.AWS.Lambda.Handler (mkHandlerWithStreaming)
import Routing.Duplex (optional, string)
import Routing.Duplex as RD
import Routing.Duplex.Generic as RDG
import Routing.Duplex.Generic.Syntax ((?))

data Endpoint = Greet { name :: Maybe String }

derive instance Generic Endpoint _
instance Show Endpoint where
  show = genericShow

endpoints :: RD.RouteDuplex' Endpoint
endpoints = RD.root $ RDG.sum
  { "Greet": "greet" ? { name: optional <<< string }
  }

type RequestBody = { ultimate_answer :: Int }

codec :: CA.JsonCodec RequestBody
codec = CA.object "RequestBody" $
  CAR.record
    { ultimate_answer: CA.int
    }

handler :: LambdaHandler APIGatewayV2
handler = mkHandlerWithStreaming { route: endpoints, router }
  where
  router = lambdaRouter \req@{ method, route } -> HTTPurple.usingCont $
    case method, route of
      Get, Greet { name } -> do
        ok $ "Hello, " <> (fromMaybe "World" name) <> "!"

      Post, Greet _ -> do
        let
          decoder = HTTPurple.JsonDecoder $
            (jsonParser >>> lmap (const unit))
              >=> (CA.decode codec >>> lmap (const unit))
        data_ <- HTTPurple.fromJson decoder req.body
        ok $ "You gave me: " <> show data_.ultimate_answer

      _, _ -> notFound
