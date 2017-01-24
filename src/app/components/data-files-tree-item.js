import Ember from 'ember';

const PER_LEVEL_PADDING_PX = 22;

const {
  run,
  computed,
  inject,
  String: {
    htmlSafe
  }
} = Ember;

export default Ember.Component.extend({
  fileBrowser: inject.service(),
  eventsBus: inject.service(),

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

  innerItemStyle: computed('level', function() {
    return htmlSafe(
      `padding-left: ${this.get('level')*PER_LEVEL_PADDING_PX}px;`
    );
  }),

  updateTruncateSizeFun: computed(function() {
    return () => this.updateTruncateSize();
  }),

  updateTruncateSize() {
    run.scheduleOnce('afterRender', this, function() {
      const $item = this.$().find('.secondary-sidebar-item');
      const $trunc = $item.find('.truncate-secondary-sidebar-item').first();
      const $secondarySidebar = $('.secondary-sidebar');
      $trunc.css(
        'max-width',
        ($secondarySidebar.width() - ($trunc.offset().left - $secondarySidebar.offset().left) - parseInt($item.css('padding-right'))) + 'px'
      );
    });
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
