import DS from 'ember-data';

import isDefaultMixinFactory from 'ember-cli-onedata-common/mixin-factories/models/is-default';

const {
  attr,
  belongsTo
} = DS;

/**
 * A configuration of a space - entry point for all options
 * that can be reached from "spaces" button in primary sidebar.
 *
 * @module models/space
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(isDefaultMixinFactory('defaultSpaceId'), {
  /** User specified name of space that will be exposed in GUI */
  name: attr('string'),

  hasViewPrivilege: attr('boolean'),

  /*** RELATIONS */

  user: belongsTo('user', { async: true }),

  /** A root directory with space files. It must be a dir-type File! */
  rootDir: belongsTo('file', { async: true }),

  /** Collection of users permissions - effectively all rows in permissions table */
  userList: belongsTo('space-user-list', { async: true }),

  /** Collection of group permissions - effectively all rows in permissions table */
  groupList: belongsTo('space-group-list', { async: true }),

  currentTransferList: belongsTo('space-transfer-list', { async: true, inverse: null }),  
  completedTransferList: belongsTo('space-transfer-list', { async: true, inverse: null }),
  providerList: belongsTo('space-provider-list', { async: true, inverse: null }),
});
