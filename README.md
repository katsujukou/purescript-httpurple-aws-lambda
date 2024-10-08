# HTTPurple AWS Lambda adapter

An adapter functionality for running HTTPurple apps on AWS Lambda

[![purs - v0.15.15](https://img.shields.io/badge/purs-v0.15.15-blue?logo=purescript)](https://github.com/purescript/purescript/releases/tag/v0.15.15)
[![CI](https://github.com/katsujukou/purescript-httpurple-aws-lambda/actions/workflows/ci.yml/badge.svg)](https://github.com/katsujukou/purescript-httpurple-aws-lambda/actions/workflows/ci.yml)

## How to use this

Say you have a HTTPurple server application like this:

```purescript
module Main where

data Endpoint
  = Greet { name :: Maybe String }

derive instance Generic Endpoint _ 
instance Show Endpoint where
  show = genericShow

endpoints :: RouteDuplex' Endpoint
endpoints = RD.root $ RDG.sum 
  { "Greet": "greet" ? { name: optional <<< string } 
  } 

router :: HTTPurple.Request Endpoint -> Aff HTTPurple.Response
router { method, route: endpoint, body } = HTTPurple.usingCont case method, endpoint of 
  Get, Greet { name } -> do
    ok $ "Hello, " <> (fromMaybe "World" name) <> "!"

  _, _ -> notFound
```

With this library, you can create AWS Lambda handler from HTTPurple router function and routing setting.

```purescript
handler :: Handler APIGatewayProxyEventV2 APIGatewayProxyResultV2
handler = mkHandler @APIGatewayV2 { route: endpoint, router }
```
