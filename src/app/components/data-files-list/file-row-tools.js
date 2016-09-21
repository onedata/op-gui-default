import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['file-row-tools'],
  classNameBindings: ['highlightClass'],

  file: null,

  metadataClass: Ember.computed('file.isEditingMetadata', function() {
    if (this.get('file.isEditingMetadata')) {
      return 'active';
    } else {
      return 'visible-on-parent-hover-25p';
    }
  }),

  actions: {
    shareFile() {
      this.sendAction('shareFile', this.get('file'));
    },
    toggleFileMetadata() {
      this.sendAction('toggleFileMetadata', this.get('file'));
    }
  }
});
