import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['file-breadcrumbs-item'],

  /**
   * @required
   * @type {FileBreadcrumbsItem}
   */
  item: undefined,

  file: Ember.computed.alias('item.file'),
  name: Ember.computed.alias('item.name'),
  isRoot: Ember.computed.alias('item.isRoot'),

  anchorId: Ember.computed('elementId', 'file.id', function() {
    return `${this.get('elementId')}-bclink-${this.get('file.id')}`;
  }),

  actions: {
    changeDir(file) {
      this.sendAction('changeDir', file);
    }
  }
});
