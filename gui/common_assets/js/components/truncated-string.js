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

  // FIXME: should register and deregister window resize event
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
    $(window).resize(changeMaxWidth);
    // FIXME: should register and deregister sidebar resize event
    this.get('eventsBus').on('secondarySidebar:resized', changeMaxWidth);

    changeMaxWidth();

    this.updateTooltipText();
  },

  updateTooltipText() {
    this.set('tooltipText', this.$().find('.truncated-string-content').text().trim());
  },

  mouseEnter() {
    this.updateTooltipText();
  }
});
