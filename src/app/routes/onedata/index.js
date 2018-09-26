import Ember from 'ember';

/**
 * An index page for application - see router.js which explain routing in this app.
 * Currently redirecting to some default path (see ``beforeModel``).
 * @module routes/index
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend({
  beforeModel() {
    this.replaceWith('onedata.data');
  }
});
