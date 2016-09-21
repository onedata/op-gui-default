import Ember from 'ember';

export default Ember.Component.extend({
  tagName: 'tr',
  classNames: ['first-level'],
  classNameBindings: ['highlightClass'],

  file: null,

  metadata: Ember.computed.alias('file.fileProperty'),

  isLoading: Ember.computed('metadata', function() {
    return !this.get('metadata');
  }),

  highlightClass: Ember.computed('file.isSelected', function() {
    return this.get('file.isSelected') ? 'active' : 'metadata-opened';
  }),
});
