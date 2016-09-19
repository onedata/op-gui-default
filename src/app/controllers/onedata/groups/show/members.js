import Ember from 'ember';

import ShowPermissionsControllerMixin from 'op-worker-gui/mixins/show-permissions-controller';

/**
 * See controllers/spaces/show-permission-base for details.
 * @module controllers/spaces/show/users
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend(ShowPermissionsControllerMixin, {
  permissionsType: 'members',
});
