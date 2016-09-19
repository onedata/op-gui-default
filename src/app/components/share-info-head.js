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

  actions: {
    openDataDir(file) {
      this.sendAction('openDataDir', file);
    }
  }
});
