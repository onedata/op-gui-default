import Ember from 'ember';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';

const {
  computed,
  computed: {
    reads,
  },
  inject: { service },
  RSVP: { reject },
} = Ember;

/**
 * Modal that displays information about DbView
 *
 * @module components/modals/db-view-modal
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  session: service(),
  store: service(),

  open: false,
  
  close: () => {},
  
  dbViewId: undefined,
  
  dbView: reads('dbViewProxy.content'),
  
  dbViewProxy: computed('dbViewId', function dbViewProxy() {
    try {
      const {
        dbViewId,
        store,
      } = this.getProperties('dbViewId', 'store');
      return PromiseObject.create({ promise: store.findRecord('db-view', dbViewId) });
    } catch (error) {
      return PromiseObject.create({ promise: reject(error) });
    }
  }),
  
  actions: {
    close() {
      this.get('close')();
    },
  },
});
