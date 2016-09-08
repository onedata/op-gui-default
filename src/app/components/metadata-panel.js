import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['metadata-panel'],

  init() {
    this._super(...arguments);
    this.set('metaBasic', Ember.Object.create({}));
    this.set('file', this.get('file'));
    this.get('metaBasic').setProperties({
      path: Ember.computed.alias('file.path'),
      shareName: 'share name',
      publicUrl: 'public url',
      spaceName: 'space name'
    });
  },

  didInsertElement() {
    this.$().find('ul').addClass('nav-tabs');
  },
});
