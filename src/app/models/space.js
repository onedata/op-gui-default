import DS from 'ember-data';
import Ember from 'ember';
import isDefaultMixinFactory from 'ember-cli-onedata-common/mixin-factories/models/is-default';

const {
  attr,
  hasMany,
  belongsTo
} = DS;

const {
  inject,
} = Ember;

/**
 * A configuration of a space - entry point for all options
 * that can be reached from "spaces" button in primary sidebar.
 *
 * @module models/space
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(isDefaultMixinFactory('defaultSpaceId'), {
  session: inject.service('session'),

  /** User specified name of space that will be exposed in GUI */
  name: attr('string'),

  hasViewPrivilege: attr('boolean'),

  /** A root directory with space files. It must be a dir-type File! */
  rootDir: belongsTo('file', {async: true}),

  /** Collection of users permissions - effectively all rows in permissions table */
  userPermissions: hasMany('spaceUserPermission', {async: true}),
  /** Collection of group permissions - effectively all rows in permissions table */
  groupPermissions: hasMany('spaceGroupPermission', {async: true}),

});
