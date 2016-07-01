/**
 * Handles all paths not handled by natively Router.
 *
 * It has two major purposes:
 * - set i18n language, if lang segment (e.g. "/en/spaces") has been found
 * - redirect to index and show "404" notify on not found routes
 *
 * @module routes/wildcard
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import RouteRejectHandler from '../mixins/route-reject-handler';

export default Ember.Route.extend(RouteRejectHandler, {
  fallbackRoute: 'index',

  rejectMessage(path) {
    return this.get('i18n').t('notFound.notifyMessage') +
      `: "${path}"`;
  },

  model(parameters) {
    let path = parameters.path;
    // example of regexp match: ['spaces/1/users', 'spaces', '/1/users']
    let pathMatch = /.*?([\w-\.]+)(.*)/.exec(path);
    let head = pathMatch[1];
    let tail = pathMatch[2];
    if (head && this.get('i18n.locales').contains(head)) {
      // TODO: allow real locale select with below code
      // this.set('i18n.locale', head);
      // TODO: in 3.0.0-beta we force English language, beacause other languages
      // are incomplete
      this.set('i18n.locale', 'en');
      this.transitionTo(tail);
    } else {
      this.actionOnReject(path);
    }
  }
});
