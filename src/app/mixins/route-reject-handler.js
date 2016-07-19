import Ember from 'ember';

/**
 * Provides methods to facilitate handling rejection of route.
 * E.g. user can type address to resource that does not exits.
 * In Routes using this mixin, it should do it like this:
 * ```
 * fallbackRoute: 'data.index',
 * model(params) {
 *   return this.handleReject(this.store.findRecord('data-space', params.data_space_id));
 * },
 * ```
 * On model reject, the application will redirect to route specified by ``fallbackRoute`` property
 * @module mixins/route-reject-handler
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create({
  notify: Ember.inject.service(),
  i18n: Ember.inject.service(),

  /**
   * Specify the route to go when model promise rejects. E.g. 'spaces.index'.
   * @abstract
   */
  fallbackRoute: 'index',

  /**
   * Optional custom message on rejection handled with 'handleReject' displayed
   * with 'notify'.
   * @param {object} error
   * @param {string} error.message
   */
  rejectMessage(error) {
    return this.get('i18n').t('common.cannotLoadResource') +
      (error ? (': ' + error.message) : '');
  },

  actionOnReject(err) {
    this.get('notify').error(this.rejectMessage(err));
    this.transitionTo(this.get('fallbackRoute'));
  },

  /**
   * Adds a rejection handler for any promise, returning to the specified by
   * 'fallbackRoute' property route.
   * @param {RSVP.Promise} promise - A promise (e.g. returned from route's model())
   * @returns {RSVP.Promise} An original promise with error handler
   */
  handleReject(promise) {
    return promise.catch((err) => this.actionOnReject(err));
  },

  // TODO: maybe specific error messages
  handleAfterModelErrors(model) {
    if (!model || model.get('isDeleted')) {
      this.actionOnReject();
    }
  }
});