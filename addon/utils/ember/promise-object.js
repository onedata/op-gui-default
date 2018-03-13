/**
 * Copy of private Ember class implementation:
 * https://www.emberjs.com/api/ember-data/2.14/classes/DS.PromiseObject
 *
 * A `PromiseObject` is an object that acts like both an `Ember.Object`
 * and a promise. When the promise is resolved, then the resulting value
 * will be set to the `PromiseObject`'s `content` property. This makes
 * it easy to create data bindings with the `PromiseObject` that will
 * be updated when the promise resolves.
 *
 * Also used as: PromiseObject somewhere in old code.
 *
 * @module utils/ember/promise-object
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  ObjectProxy,
  PromiseProxyMixin,
} = Ember;

export default ObjectProxy.extend(PromiseProxyMixin);
