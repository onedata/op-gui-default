export default {
  name: 'register-download-error-handler',
  after: ['store', 'i18n'],
  initialize: function(container) {
    const store = container.lookup('service:store');
    const i18n = container.lookup('service:i18n');
    const messageBox = container.lookup('service:messageBox');
    const serverMessagesHandler = container.lookup('service:serverMessagesHandler');

    const showMessageFun = (fileName, errorMessage) => {
      fileName = (fileName || i18n.t('common.unknown'));
      messageBox.open({
        metadata: {name: 'file-download-failure'},
        type: 'error',
        title: i18n.t('initializers.registerDownloadErrorHandler.title'),
        message: i18n.t('initializers.registerDownloadErrorHandler.message', {
          fileName: fileName,
          errorMessage: errorMessage
        })
      });
    };

    serverMessagesHandler.onMessage('fileDownloadError', (data) => {
      const errorMessage = data.error.msg;
      let fileName;
      // a fileId is known - so we can read its name fron model
      if (data.fileId) {
        const promise = store.find('file', data.fileId);
        promise.then(
          (file) => {
            fileName = file.get('name');
          }
        );
        promise.finally(
          () => {
            showMessageFun(fileName, errorMessage);
          }
        );
      } else {
        // file not found in model - show modal with unknown name
        showMessageFun(undefined, errorMessage);
      }

    });
  }
};
