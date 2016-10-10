import Ember from 'ember';

const PER_LEVEL_PADDING_PX = 22;

export default Ember.Component.extend({
  fileBrowser: Ember.inject.service(),
  eventsBus: Ember.inject.service(),

  tagName: 'li',
  classNames: ['data-files-list-item'],

  /**
   * To inject.
   * @type {Number}
   */
  level: null,

  didInsertElement() {
    this.updateTruncateSize();
    this.get('eventsBus').on('secondarySidebar:resized', this.get('updateTruncateSizeFun'));
  },

  willDestroyElement() {
    this.get('eventsBus').off('secondarySidebar:resized', this.get('updateTruncateSizeFun'));
  },

  innerItemStyle: Ember.computed('level', function() {
    return Ember.String.htmlSafe(
      `padding-left: ${this.get('level')*PER_LEVEL_PADDING_PX}px;`
    );
  }),

  updateTruncateSizeFun: Ember.computed(function() {
    return () => this.updateTruncateSize();
  }),

  updateTruncateSize() {
    const $item = this.$().find('.secondary-sidebar-item');
    const $trunc = $item.find('.truncate-secondary-sidebar-item').first();
    const $secondarySidebar = $('.secondary-sidebar');
    $trunc.css(
      'max-width',
      ($secondarySidebar.width() - ($trunc.offset().left - $secondarySidebar.offset().left) - parseInt($item.css('padding-right'))) + 'px'
    );
  },

  actions: {
    toggleDir() {
      this.set('file.isExpanded', !this.get('file.isExpanded'));
    },

    browseDir() {
      if (this.get('file.isDir')) {
        this.sendAction('openDirInBrowser', this.get('file'));
      } else {
        console.error(`Tried to browse a file in file brower (should be dir): ${this.get('file.id')}`);
      }
    },

    openDirInBrowser(file) {
      this.sendAction('openDirInBrowser', file);
    },
  }
});
