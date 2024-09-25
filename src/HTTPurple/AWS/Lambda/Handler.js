export const asStreamingEnabledLambdaHandler = function (handler) {
  return awslambda.streamifyResponse(handler);
}