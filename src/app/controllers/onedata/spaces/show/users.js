import Ember from 'ember';
import showPermissionsMixinFactory from 'op-worker-gui/mixin-factories/controllers/show-permissions';

/**
 * See controllers/spaces/show-permission-base for details.
 * @module controllers/onedata/spaces/show/users
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend(showPermissionsMixinFactory('user'), {});
