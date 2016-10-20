import DS from 'ember-data';

/**
 * Represents a public handle for resource (currently a public shared dir).
 * Version for public view.
 * @module models/handle-public
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  handleService: DS.belongsTo('handleService', {inverse: null, async: true}),
  metadataString: DS.attr('string'),
  publicHandle: DS.attr('string'),
});
