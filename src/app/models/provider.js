import DS from 'ember-data';

/**
 * Information about provider - created especially for file-distribution model.
 * @module models/provider
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: DS.attr('string')
});
