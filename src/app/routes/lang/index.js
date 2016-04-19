import Ember from 'ember';

/**
 * Index page with language selection support.
 * @module routes/lang/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  afterModel() {
    this.transitionTo('data', this.get('i18n.locale'));
  }
});
