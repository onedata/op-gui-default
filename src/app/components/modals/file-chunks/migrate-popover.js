/**
 * FIXME: doc
 * @module components/modals/file-chunks/migrate-popover
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import bindFloater from 'ember-cli-onedata-common/utils/bind-floater';

const {
  Component,
  computed,
  get,
} = Ember;

export default Component.extend({
  tagName: 'ul',
  attributeBindings: ['style'],
  classNames: [
    'dropdown-menu',
    'dropdown-menu-right',
    'dropdown-menu-list', 
    'space-dropdown-menu', 
    'dropdown-menu-settings', 
    'migrate-popover'
  ],
  
  /**
   * @virtual
   * @type {Array<Provider>}
   */
  providers: undefined,
  
  /**
   * @virtual
   * @type {Provider}
   */
  sourceProvider: undefined,
  
  /**
   * @virtual
   * @type {Function}
   */
  startMigration: () => {},
  
  bindSelector: computed('sourceProvider.id', function () {
    return `.provider-row-${this.get('sourceProvider.id')}`;
  }),

  /**
   * @type {Array<Provider>|undefined}
   */
  visibleProviders: computed('providers.[]', 'sourceProvider', function () {
    const {
      providers,
      sourceProvider,
    } = this.getProperties(
       'providers',
       'sourceProvider'
    );
    if (providers || sourceProvider) {
      const source = get(sourceProvider, 'id');
      return providers.filter(p => get(p, 'id') !== source);
    }
  }),

  didInsertElement() {
    const bindSelector = this.get('bindSelector');
    if (bindSelector) {
      bindFloater(this.$(), $(bindSelector));
    }
  },

  actions: {
    startMigration(destination) {
      this.startMigration(this.get('sourceProvider.id'), destination);
    },
  },
});
