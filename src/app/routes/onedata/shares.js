import Ember from 'ember';
import userCollectionModel from 'ember-cli-onedata-common/mixin-factories/routes/user-collection-model';


/**
 * Load user's Shares list.
 * 
 * @module routes/onedata/shares
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend(userCollectionModel('shares'), {
  mainRouteName: 'shares',

  beforeModel() {
    // TODO: this causes full reload of shared files, try to use unloadAll
    this.store.peekAll('file-shared').forEach(r => r.reload());
  },

  actions: {
    /** Show Share */
    goToShare(share) {
      this.transitionTo('onedata.shares.show', share);
    },
  }
});
