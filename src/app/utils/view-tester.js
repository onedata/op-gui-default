/**
 * Instance of this class is bound to some `$container`.
 * We can now test if some child element is visible in viewport.
 * Remember to use `.destroy()` when watcher is not needed!
 *
 * @module utils/view-tester
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

export default class ViewTester {
  /**
   * @param {HTMLElement} container 
   */
  constructor($container) {
    this.containerTop = $container.offset().top;
    this.containerBottom = this.containerTop + $container[0].clientHeight;
  }

  /**
   * @param {HTMLElement} elem
   */
  isInView(elem) {
    const elemTop = $(elem).offset().top;
    const elemBottom = elemTop + elem.offsetHeight;

    return (elemTop <= this.containerBottom) && (elemBottom >= this.containerTop);
  }
  
  destroy() {
    this.$container.off('scroll', this._scrollHandler);
  }
}