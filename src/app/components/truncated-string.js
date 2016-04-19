import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'div',
  classNames: ['truncate', 'truncated-string'],

  parentSelector: null,
  shrinkBy: 0,

  didInsertElement() {
    let parentSelector = this.get('parentSelector');
    let parent = parentSelector ? this.$().closest(parentSelector) : this.$().parent();
    let shrinkBy = this.get('shrinkBy') || 0;
    let changeMaxWidth = (/*event*/) => {
      let maxWidth = parent.width();
      console.debug(`truncated string - set max width to: ${maxWidth}`);
      this.$().css({
        maxWidth: (parseInt(maxWidth) - shrinkBy)
      });
    };
    $(window).resize(changeMaxWidth);
    changeMaxWidth();
  }
});
