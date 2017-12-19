/**
 * Copy of private Ember class implementation:
 * https://www.emberjs.com/api/ember-data/2.14/classes/DS.PromiseArray
 *
 * A PromiseArray is an object that acts like both an Ember.Array and a promise.
 * When the promise is resolved the resulting value will be set to the
 * PromiseArray's content property. This makes it easy to create data bindings
 * with the PromiseArray that will be updated when the promise resolves.
 *
 * @module utils/ember/promise-array
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  ArrayProxy,
  PromiseProxyMixin,
} = Ember;

export default ArrayProxy.extend(PromiseProxyMixin);
