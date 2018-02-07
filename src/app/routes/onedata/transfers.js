/**
 * A Transfers page from main-menu.
 *
 * Lists Spaces for user, every space has a transfers view for this space.
 * @module routes/onedata/transfers
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import userCollectionModel from 'ember-cli-onedata-common/mixin-factories/routes/user-collection-model';

const {
   Route,
   inject: { service },
} = Ember;

export default Route.extend(userCollectionModel('spaces', { nonEmpty: true }), {
  mainRouteName: 'transfers',
  
  secondaryMenu: service(),
  
  beforeModel() {
    this._super(...arguments);
    this.set('secondaryMenu.component', null);
  },
});
