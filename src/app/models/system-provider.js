import DS from 'ember-data';

/**
 * A single Onedata provider representation
 * Created originally for file-distribution model.
 * @module models/system-provider
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  name: DS.attr('string')
});

// -- FIXME: mocks ---

export const mock1 = {
  id: 'p1',
  name: 'Provider One',
  latitude: 10,
  longitude: 20,
  status: 'online',
};
export const mock2 = {
  id: 'p2',
  name: 'Provider Two',
  latitude: 40,
  longitude: 60,
  status: 'online',
};
export const mock3 = {
  id: 'p3',
  name: 'Provider Three',
  latitude: -20,
  longitude: 30,
  status: 'online',
};
export const mock4 = {
  id: 'p4',
  name: 'Provider Four',
  latitude: 50,
  longitude: -50,
  status: 'pending',
};
