/**
 * A localstorage adapter.
 * Using DS.LSAdapter class which should be defined in vendor JS when using
 * ember-localstorage-adapter Ember and Bower package.
 * @module adapters/base/localstorage
 */

import LSAdapter from 'ember-localstorage-adapter';

export default LSAdapter.extend({
  namespace: 'oneprovider'
});
