import DS from 'ember-data';

/**
 * A container for shared files.
 *
 * @module models/share
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: DS.attr('string'),
  file: DS.hasMany('file', {async: true}),
  shareType: DS.attr('string'),
  publicAccess: DS.attr('boolean'),
  // TODO: list of users/group/emails
});
