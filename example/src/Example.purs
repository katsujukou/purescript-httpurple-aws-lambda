module Example where

import Example.Basic as Basic
import HTTPurple.AWS.Lambda (APIGatewayV2, LambdaHandler)

basic :: LambdaHandler APIGatewayV2
basic = Basic.handler
