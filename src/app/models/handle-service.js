import DS from 'ember-data';

const {
  attr,
  belongsTo
} = DS;

/**
 * Represents a service that allows to publish DOI handles.
 * @module models/handle-service
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: attr('string'),

  user: belongsTo('user', { async: true }),
});
