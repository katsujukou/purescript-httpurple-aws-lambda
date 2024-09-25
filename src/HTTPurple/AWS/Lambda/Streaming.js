export const setStatusCode = responseStream => code => () => {
  responseStream.statusCode = code;
}

export const setHeader = responseStream => key => value => () => {
  responseStream.setHeader(key, value);
} 