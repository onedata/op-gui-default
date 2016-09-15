import Ember from 'ember';

export default Ember.Component.extend({
  notify: Ember.inject.service(),

  classNames: ['metadata-panel'],

  data: null,

  isSaving: false,

  init() {
    this._super(...arguments);
  },

  didInsertElement() {
    this.$().find('ul').addClass('nav-tabs');
  },

  actions: {
    saveAll() {
      // FIXME: validate before save
      // FIXME: translations
      this.set('isSaving', true);
      const p = this.get('data.content').save();
      p.then(() => {
        // TODO: file name
        this.get('notify').info('Metadata saved successfully');
      });
      p.catch((error) => {
        this.get('notify').error('Cannot save metadata: ' + error && error.message || 'unknown error');
      });
      p.finally(() => this.set('isSaving', false));
    }
  }
});
