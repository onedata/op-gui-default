import DS from 'ember-data';

const {
  belongsTo,
  hasMany
} = DS;

const ASYNC = { async: true };

/**
 * FIXME
 * @module models/group-group-list
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  group: belongsTo('group', ASYNC),
  permissions: hasMany('group-group-permission', ASYNC),
});
