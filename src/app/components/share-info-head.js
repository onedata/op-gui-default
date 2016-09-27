import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['share-info-head'],

  /**
   * To inject - a Share object whose basic info will be displayed
   * @type {Share}
   */
  share: null,

  /**
   * Alias
   * @type {File}
   */
  file: Ember.computed.oneWay('share.file'),

  /**
   *
   */
  handle: Ember.computed.oneWay('share.handle.content'),

  isPublished: Ember.computed('share.hasHandle', function() {
    return !!this.get('share.hasHandle');
  }),

  actions: {
    openDataDir(file) {
      this.sendAction('openDataDir', file);
    },
    publishShare() {
      this.sendAction('publishShare');
    }
  }
});
