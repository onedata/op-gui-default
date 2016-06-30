export default {
  connectionClosed: {
    title: 'WebSocket connection error',
    message: 'WebSocket connection has been closed',
    messageNotOpened: 'WebSocket connection could not be estabilished',
    reconnectWait: 'Will try to reconnect in {{secs}} seconds...',
    reconnecting: 'Reconnecting...',
    reasons: {
      notOpened: 'WebSocket connection could not be estabilished',
      // code: 1006
      abnormal: 'closed unexpectedly, please try refreshing browser page',
      // all other codes
      unknown: 'unhandled error, see browser logs for details',
      // special message for Safari when connection cannot be opened
      safariCert: 'The connection cannot be established because either' +
        ' server is unavailable or the SSL certificate is not valid (e.g. it is self-signed).'
    }
  }
};
