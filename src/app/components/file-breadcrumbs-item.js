import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['file-breadcrumbs-item'],

  useAsRoot: undefined,

  isRoot: Ember.computed('file.hasParent', 'useAsRoot', function() {
    return this.get('useAsRoot') || !this.get('file.hasParent');
  }),

  anchorId: Ember.computed('elementId', 'file.id', function() {
    return `${this.get('elementId')}-bclink-${this.get('file.id')}`;
  }),

  actions: {
    changeDir(file) {
      this.sendAction('changeDir', file);
    }
  }
});
