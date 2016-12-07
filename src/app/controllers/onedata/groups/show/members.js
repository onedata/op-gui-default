import Ember from 'ember';

import ShowPermissionsControllerMixin from 'op-worker-gui/mixins/show-permissions-controller';

/**
 * See mixins/show-permission-controller for details.
 * @module controllers/onedata/groups/show/users
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend(ShowPermissionsControllerMixin, {
  permissionsType: 'members',
});
