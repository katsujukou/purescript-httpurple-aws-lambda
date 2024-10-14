module Example where

import Example.Basic as Basic
import Example.ExtMiddleware as ExtMiddleware
import HTTPurple.AWS.Lambda (APIGatewayV2, LambdaHandler)

basic :: LambdaHandler APIGatewayV2
basic = Basic.handler

extMiddleware :: LambdaHandler APIGatewayV2
extMiddleware = ExtMiddleware.handler