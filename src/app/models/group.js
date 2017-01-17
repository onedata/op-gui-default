import DS from 'ember-data';
import isDefaultMixinFactory from 'ember-cli-onedata-common/mixin-factories/models/is-default';

const {
  attr,
  hasMany,
  belongsTo
} = DS;

/**
 * A group in system - model for groups/ routes.
 * This must be a group that is accessible for current user.
 * See also models/system-group
 *
 * @module models/group
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(isDefaultMixinFactory('defaultSpaceId'), {
  /** User specified name of space that will be exposed in GUI */
  name: attr('string'),

  hasViewPrivilege: attr('boolean'),

  /*** RELATIONS */

  user: belongsTo('user', { async: true }),

  /** Collection of users permissions - effectively all rows in permissions table */
  userList: belongsTo('group-user-list', { async: true }),
  
  /** Collection of group permissions - effectively all rows in permissions table */
  groupList: belongsTo('group-group-list', { async: true }),

  parentGroups: hasMany('group', {async: true, inverse: 'childGroups'}),
  childGroups: hasMany('group', {async: true, inverse: 'parentGroups'}),
});
