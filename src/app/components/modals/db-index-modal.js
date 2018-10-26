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
 * Modal that displays information about DbIndex
 *
 * @module components/modals/db-index-modal
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  session: service(),
  store: service(),

  open: false,
  
  close: () => {},
  
  dbIndexId: undefined,
  
  dbIndex: reads('dbIndexProxy.content'),
  
  dbIndexProxy: computed('dbIndexId', function dbIndexProxy() {
    try {
      const {
        dbIndexId,
        store,
      } = this.getProperties('dbIndexId', 'store');
      return PromiseObject.create({ promise: store.findRecord('db-index', dbIndexId) });
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
