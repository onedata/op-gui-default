/**
 * Instance of this class is bound to some `$container`.
 * When scrolling the container, we check which of elements (filtered by selector)
 * inside container are visible in view port.
 *
 * @module utils/list-watcher
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import ViewTester from 'op-worker-gui/utils/view-tester';

export default class ListWatcher {
  /**
   * @param {jQuery} container 
   * @param {string} itemsSelector 
   * @param {function} callback
   */
  constructor($container, itemsSelector, callback) {
    this.$container = $container;
    this.itemsSelector = itemsSelector;
    this.callback = callback;
    this._scrollHandler = this.scrollHandler.bind(this);
    this.viewTester = new ViewTester($container);

    $container.on('scroll', this._scrollHandler);
  }

  scrollHandler() {
    const items = this.$container.find(this.itemsSelector).toArray();
    let visibleFragment = false;
    const visibleElements = [];
    for (let i = 0; i < items.length; i++) {
      const item = items[i];
      const visible = this.viewTester.isInView(item);
      if (visible) {
        visibleElements.push(item);
        visibleFragment = true;
      } else if (visibleFragment) {
        break;
      }
    }
    this.callback(visibleElements);
  }
}