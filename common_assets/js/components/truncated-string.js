import Ember from 'ember';

export default Ember.Component.extend({
  eventsBus: Ember.inject.service(),

  tagName: 'div',
  classNames: ['truncated-string'],

  parentSelector: null,
  shrinkBy: 0,

  // disabled by default due to tooltip update bugs, and lack of truncate detect
  /**
    If true, shows Bootstrap tooltip with this component yielded text
    If false, uses HTML "title" property with this component yielded text
  */
  showTooltip: false,

  /**
   * Function for updating max width
   * @private
   */
  __changeMaxWidthFun: null,

  didInsertElement() {
    let parentSelector = this.get('parentSelector');
    let parent = parentSelector ? this.$().closest(parentSelector) : this.$().parent();
    let shrinkBy = this.get('shrinkBy') || 0;
    let $element = this.$();
    let changeMaxWidth = (/*event*/) => {
      let maxWidth = parent.width();
      $element.css({
        maxWidth: (parseInt(maxWidth) - shrinkBy)
      });
    };

    this.set('__changeMaxWidthFun', changeMaxWidth);

    $(window).resize(changeMaxWidth);
    if (this.get('eventsBus')) {
      this.get('eventsBus').on('secondarySidebar:resized', changeMaxWidth);
    }

    changeMaxWidth();

    this.updateTooltipText();
  },

  willDestroyElement() {
    let changeMaxWidth = this.get('__changeMaxWidthFun');
    $(window).off('resize', changeMaxWidth);
    if (this.get('eventsBus')) {
      this.get('eventsBus').off('secondarySidebar:resized', changeMaxWidth);
    }
  },

  updateTooltipText() {
    this.set('tooltipText', this.$().find('.truncated-string-content').text().trim());
  },

  mouseEnter() {
    this.updateTooltipText();
  }
});
