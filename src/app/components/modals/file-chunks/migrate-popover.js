/**
 * A menu in form of dropdown-menu-list with destination providers for
 * data migration.
 * 
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
    'dropdown-menu-settings', 
    'migrate-popover'
  ],
  
  /**
   * @virtual
   * @type {Array<PromiseObject<Provider>>}
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
  
  /**
   * @virtual 
   * @type {Array<string>}
   */
  disabledProviderIds: undefined,
  
  providersSorting: ['name'],
  providersSorted: computed.sort('providers', 'providersSorting'),
  
  bindSelector: computed('sourceProvider.id', function () {
    return `.provider-row-${this.get('sourceProvider.id')} .btn-migrate`;
  }),

  /**
   * @type {Array<Provider>|undefined}
   */
  visibleProviders: computed('providersSorted.[]', 'sourceProvider', function () {
    const {
      providersSorted,
      sourceProvider,
    } = this.getProperties(
       'providersSorted',
       'sourceProvider'
    );
    if (providersSorted && sourceProvider) {
      const source = get(sourceProvider, 'id');
      return providersSorted.filter(p => get(p, 'id') !== source);
    }
  }),
  
  didInsertElement() {
    const bindSelector = this.get('bindSelector');
    if (bindSelector) {
      const bindElement = $(bindSelector);
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
      this.get('startMigration')(this.get('sourceProvider.id'), destination, {
        transfersPending: this.get('migrationTransfersPending'),
      });
    },
  },
});
