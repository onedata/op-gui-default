/**
 * Content of file browser view then there is error loading space/dir
 * 
 * @module components/space-content-error
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  inject: { service },
  computed,
  computed: { reads },
} = Ember;

export default Component.extend({
  session: service(),
  router: service(),

  /**
   * @virtual
   * Properties: type, space, provider (opt), providers (opt), transition (opt)
   * @type {object}
   */
  error: undefined,

  i18nPrefix: 'components.spaceContentError.',

  onezoneUrl: reads('session.sessionDetails.onezoneURL'),
  providerApiOrigin: reads('error.provider.apiOrigin'),
  space: reads('error.space'),
  providers: reads('error.providers'),

  spaceSupportUrl: computed('onezoneURL', 'space.id', function spaceSupportUrl() {
    return `${this.get('onezoneUrl')}/#/onedata/spaces/${this.get('space.id')}/support`;
  }),

  actions: {
    refreshRoute() {
      this.get('error.transition').retry();
    },
  },
});
