export const setStatusCode = code => responseStream => () => {
  responseStream.statusCode = code;
}

export const setHeader = key => value => responseStream => () => {
  responseStream.setHeader(key, value);
} 