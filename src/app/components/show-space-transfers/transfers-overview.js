/**
 * Shows overview data for all transfers for the space
 * 
 * Automatic sticky on scrolling
 *
 * @module components/show-space-transfers/transfers-overview
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

const {
  Component,
  computed,
  observer,
  String: { htmlSafe },
  run: { debounce, next },
} = Ember;

export default Component.extend({
  classNames: ['transfers-overview', 'row', 'row-spacing'],
  attributeBindings: ['style'],

  /**
   * Set by scroll/resize event handler
   * @type {boolean}
   */
  stickyOverview: undefined,

  /**
   * @type {number}
   */
  contentScrollTop: undefined,

  /**
   * @type {boolean}
   */
  overviewExpanded: false,

  /**
   * FIXME: doc
   * @type {number}
   */
  initialHandlerTop: undefined,
  
  providers: undefined,
  providerTransferConnections: undefined,
  destinationProviderIds: undefined,
  sourceProviderIds: undefined,
  providersColors: undefined,
  
  /**
   * What speed (input, output) should be presented per provider
   * on throughput chart.
   * One of: out, in
   * @type {string}
   */
  transfersPieChartDirection: 'out',
  
  style: computed('stickyOverview', function getStyle() {
    if (this.get('stickyOverview')) {
      const height = this.$('.row-active-transfers').outerHeight();
      return htmlSafe(`margin-top: ${height}px;`);
    } else {
      return htmlSafe();
    }
  }),

  stickyOverviewStyle: computed('stickyOverview', 'overviewExpanded', function getStickyOverviewStyle() {
    if (this.get('stickyOverview')) {
      const {
        contentScrollTop,
        overviewExpanded,
      } = this.getProperties(
        'contentScrollTop',
        'overviewExpanded'
      );
      const $rowOverview = this.$('.row-overview');
      const top = overviewExpanded ? contentScrollTop : (contentScrollTop -
        $rowOverview.height());
      const left = $rowOverview.offset().left;
      const right = window.innerWidth - (left + $rowOverview.width());
      // FIXME: left and right should be recomputed on window size change
      const style = `top: ${top}px; left: ${left}px; right: ${right}px;`;
      return htmlSafe(style);
    }
  }),
  
  observeOverviewExpanded: observer('overviewExpanded', function () {
    if (!this.get('overviewExpanded')) {
      next(() => {
        this.computeSticky();
      });
    }
  }),

  didInsertElement() {
    const $contentScroll = $('#content-scroll');
    this.initSticky($contentScroll);
    $contentScroll.on(
      this.eventName('scroll'),
      () => debounce(() => safeExec(this, 'computeSticky'), 0)
    );
  },

  willDestroyElement() {
    $('#content-scroll').off(this.eventName('scroll'));
  },

  initSticky($contentScroll) {
    const $rowExpandHandler = this.$('.row-expand-handler');
    if ($rowExpandHandler) {
      this.set('initialHandlerTop', $rowExpandHandler.offset().top);
    }
    if ($contentScroll) {
      this.set('contentScrollTop', $contentScroll.offset().top);
    }
  },
  
  /**
   * Check if overview panel should become/stay sticky
   * Should be invoked on view change events (scroll, resize)
   */
  computeSticky() {
    const {
      initialHandlerTop,
      contentScrollTop,
    } = this.getProperties('initialHandlerTop', 'contentScrollTop');
    const contentScroll = document.getElementById('content-scroll');
    const sticky = this.get('overviewExpanded') ?
      (contentScroll.scrollTop !== 0) :
      (initialHandlerTop - contentScrollTop <= contentScroll.scrollTop);
    if (!sticky) {
      this.set('overviewExpanded', false);
    }
    this.set('stickyOverview', sticky);
  },

  /**
   * Event name for jQuery associated with this component
   * @param {string} type type, aka. `eventName` (eg. scroll)
   * @returns {string}
   */
  eventName(type) {
    return `${type}.${this.elementId}`;
  },

  actions: {
    toggleOverview() {
      this.toggleProperty('overviewExpanded');
    },
  },
});
