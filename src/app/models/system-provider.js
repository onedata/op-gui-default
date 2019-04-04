/**
 * A single Onedata provider representation
 * Created originally for file-distribution model.
 * @module models/system-provider
 * @author Jakub Liput
 * @copyright (C) 2016-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import DS from 'ember-data';

const {
  Model,
  attr,
} = DS;

const {
  computed,
  computed: { equal },
} = Ember;

export default Model.extend({
  name: attr('string'),
  latitude: attr('number'),
  longitude: attr('number'),
  status: attr('string'),
  cluster: attr('string'),
  domain: attr('string'),

  online: equal('status', 'online'),

  apiOrigin: computed('domain', function apiOrigin() {
    return `https://${this.get('domain')}`;
  }),
});
