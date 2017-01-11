import DS from 'ember-data';

const {
  belongsTo,
  hasMany
} = DS;

const ASYNC = { async: true };

/**
 * FIXME
 * @module models/space-user-list
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  space: belongsTo('space', ASYNC),
  permissions: hasMany('space-user-permission', ASYNC),
});
