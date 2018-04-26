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
  observer,
  String: { htmlSafe },
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
   * Position of expand handler on overview component initialization
   * @type {number}
   */
  initialHandlerTop: undefined,

  /**
   * Id of provider selected in throughput chart.
   * @type {string}
   */
  selectedTransferStatProviderId: null,

  /**
   * @type {string}
   */
  throughputTransferType: 'all',
  
  providers: undefined,
  providerTransferConnections: undefined,
  destinationProviderIds: undefined,
  sourceProviderIds: undefined,
  providersColors: undefined,
    
  _window: window,
  
  _mobileMode: false,
  
  changeStyle() {
    let style;
    if (this.get('stickyOverview')) {      
      const $rowActiveTransfers = this.$('.row-active-transfers');
      const height = $rowActiveTransfers.outerHeight();
      const width = this.$().parents('.show-space-transfers').innerWidth();
      style = htmlSafe(`height: ${height}px; width: ${width}px;`);
    } else {
      style = htmlSafe();
    }
    this.set('style', style);
  },
  
  changeStickyOverviewStyle() {
    let stickyOverviewStyle;
    if (this.get('stickyOverview')) {
      const {
        contentScrollTop,
        overviewExpanded,
      } = this.getProperties(
        'contentScrollTop',
        'overviewExpanded'
      );
      const $rowOverview = this.$('.row-overview');
      const top = (overviewExpanded ?
        contentScrollTop :
        contentScrollTop - $rowOverview.height()
      );
      const left = this.$().offset().left;
      const right = window.innerWidth - (left + this.$().width());
      const style = `top: ${top}px; left: ${left}px; right: ${right}px;`;
      stickyOverviewStyle = htmlSafe(style);
    }
    this.set('stickyOverviewStyle', stickyOverviewStyle);
  },
  
  stickyOverviewChanged: observer('stickyOverview', function () {
    this.changeStyle();
    this.changeStickyOverviewStyle();
  }),

  overviewExpandedChanged: observer('overviewExpanded', function () {
    if (!this.get('overviewExpanded')) {
      this.computeSticky();
    }
    this.changeStickyOverviewStyle();
  }),

  init() {
    this._super(...arguments);
    // enable observers
    this.getProperties('stickyOverview', 'overviewExpanded');
  },
  
  didInsertElement() {
    const $contentScroll = $('#content-scroll');
    this.initSticky($contentScroll);
    $contentScroll.on(
      this.eventName('scroll'),
      () => safeExec(this, 'computeSticky')
    );
    $(window).on(
      this.eventName('resize'),
      () => safeExec(this, () => {
        this.updateMobileMode();
        this.computeSticky();
        this.changeStyle();
        this.changeStickyOverviewStyle();
      })
    );
  },

  willDestroyElement() {
    $('#content-scroll').off(this.eventName('scroll'));
    $(window).off(this.eventName('resize'));
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
      stickyOverview,
      _mobileMode,
    } = this.getProperties(
      'initialHandlerTop',
      'contentScrollTop',
      'stickyOverview',
      '_mobileMode'
    );
    let sticky;
    if (_mobileMode) {
      sticky = false;
    } else {
      const contentScroll = document.getElementById('content-scroll');
      sticky = this.get('overviewExpanded') ?
        (contentScroll.scrollTop !== 0) :
        (initialHandlerTop - contentScrollTop <= contentScroll.scrollTop);
    }
    if (!sticky && stickyOverview) {
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

  /**
   * Window resize event handler.
   * @type {Ember.ComputedProperty<Function>}
   */
  updateMobileMode() {
      this.set('_mobileMode', this.get('_window.innerWidth') < 1261);
  },
  
  actions: {
    selectTransferStatProvider(providerId) {
      this.set('selectedTransferStatProviderId', providerId);
    },
    
    selectThroughputTransferType(type) {
      this.set('throughputTransferType', type);
    },

    toggleOverview() {
      this.toggleProperty('overviewExpanded');
    },

    // TODO: this not prevents bad focus when 
    stickyFocused() {
      if (!this.get('stickyOverview')) {
        this.$()[0].setAttribute('tabindex', 0);
      }
    },
  },
});
