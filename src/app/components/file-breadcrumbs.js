import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['file-breadcrumbs'],

  /**
   * File, which path to will be presented.
   * Can be a directory of course.
   * @type {File}
   */
  file: null,

  dirsPath: Ember.computed.oneWay('file.dirsPath'),

  isLoading: Ember.computed('dirsPath', function() {
    return !this.get('dirsPath');
  }),

  actions: {
    changeDir(file) {
      this.sendAction('changeDir', file);
    }
  }
});
