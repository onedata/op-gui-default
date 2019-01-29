import DS from 'ember-data';

const {
  attr,
} = DS;

/**
 * A single Onedata provider representation
 * Created originally for file-distribution model.
 * @module models/system-provider
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: attr('string'),
  latitude: attr('number'),
  longitude: attr('number'),
  status: attr('string'),
  cluster: attr('string'),
});
