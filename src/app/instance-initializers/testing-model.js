import ENV from 'op-worker-gui/config/environment';

export default {
  name: 'testing-model',
  initialize: function(application) {
    if (ENV.environment !== 'localstorage') {
      console.debug(`Skipping testing-model creation because environment is not localstorage`);
    } else {
      let store = application.lookup('service:store');

      /// spaces

      let space1 = store.createRecord('space', {
        id: 1,
        name: 'My space',
        isDefault: false
      });


      let space2 = store.createRecord('space', {
        id: 2,
        name: 'Second space',
        isDefault: true
      });

      /// save all records
      space1.save();
      space2.save();
    }
  }
};
