import Ember from 'ember';

export default Ember.Component.extend({
  notify: Ember.inject.service(),

  classNames: ['metadata-panel'],

  /**
   * A metadata record to edit in this panel.
   * To inject.
   * @type {Ember.ObjectProxy<FileProperty>}
   */
  metadataProxy: null,

  isSaving: false,

  init() {
    this._super(...arguments);
  },

  metadata: Ember.computed.oneWay('metadataProxy.content'),

  metadataIsModified: Ember.computed('metadata.hasDirtyAttributes', function() {
    return this.get('metadata.hasDirtyAttributes');
  }),

  saveEnabled: Ember.computed('metadataIsModified', 'isSaving',
    function() {
      return this.get('metadataIsModified') && !this.get('isSaving');
    }
  ),

  didInsertElement() {
    this.$().find('ul').addClass('nav-tabs');
  },

  actions: {
    saveAll() {
      // FIXME: validate before save
      // FIXME: translations
      this.set('isSaving', true);
      const p = this.get('metadata').save();
      p.then(() => {
        // TODO: file name
        this.get('notify').info('Metadata saved successfully');
      });
      p.catch((error) => {
        this.get('notify').error('Cannot save metadata: ' + error && error.message || 'unknown error');
      });
      p.finally(() => this.set('isSaving', false));
    },

    discardChanges() {
      if (this.get('metadata.isNew')) {
        // we created new local record, and by discarding changes, we remove it completely
        this.get('metadata').deleteRecord();
        this.sendAction('closeMetadataPanel');
      } else {
        this.get('metadata').rollbackAttributes();
      }
    },
  }
});
