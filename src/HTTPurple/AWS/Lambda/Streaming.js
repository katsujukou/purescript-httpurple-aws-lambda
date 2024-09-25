export const withMetadata = responseStream => metadata => () => {
  return awslambda.HttpResponseStream.from(responseStream, metadata);
}