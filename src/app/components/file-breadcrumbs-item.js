import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['file-breadcrumbs-item'],

  anchorId: Ember.computed('elementId', 'file.id', function() {
    return `${this.get('elementId')}-bclink-${this.get('file.id')}`;
  }),

  actions: {
    changeDir(file) {
      this.sendAction('changeDir', file);
    }
  }
});
