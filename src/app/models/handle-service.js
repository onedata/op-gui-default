import DS from 'ember-data';

/**
 * Represents a service that allows to publish DOI handles.
 * @module models/handle-service
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: DS.attr('string'),
});
