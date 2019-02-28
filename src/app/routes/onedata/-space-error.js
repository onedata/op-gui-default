/**
 * Renders error of space loading for various routes
 * 
 * @module routes/onedata/-space-error
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Route,
} = Ember;

export default Route.extend({
  renderTemplate() {
    this.render('disabled', { outlet: 'data-space-sidebar' });
    this.render('onedata/-space-content-error', { outlet: 'data-content-scroll' });
    this.render('disabled', {
      into: 'onedata',
      outlet: 'toolbar'
    });
  },
});
