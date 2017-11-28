/**
 * FIXME: doc
 * @module components/modals/file-chunks/migrate-popover
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import ClickOutside from 'ember-click-outside/mixins/click-outside';
import bindFloater from 'ember-cli-onedata-common/utils/bind-floater';

const {
  Component,
  computed,
  get,
  run,
} = Ember;

export default Component.extend(ClickOutside, {
  tagName: 'ul',
  attributeBindings: ['style'],
  classNames: [
    'dropdown-menu',
    'open',
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
  
  /**
   * @virtual
   * @type {Function}
   */
  close: () => {},
  
  bindSelector: computed('sourceProvider.id', function () {
    return `.provider-row-${this.get('sourceProvider.id')} .btn-migrate`;
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
    const bindElement = $(bindSelector);
    if (bindSelector) {
      bindFloater(this.$(), bindElement, {
        offsetY: -bindElement.height() / 2 + 3,
        stackingContext: this.$().parents('.modal-dialog').get(0)
      });
    }
    run.next(this, this.addClickOutsideListener);
  },
  
  willDestroyElement() {
    this.removeClickOutsideListener();
  },
  
  clickOutside() {
    this.close();
  },
  
  actions: {
    startMigration(destination) {
      this.startMigration(this.get('sourceProvider.id'), destination);
    },
  },
});
