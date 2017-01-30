import Ember from 'ember';

const {
  inject,
  computed,
  run
} = Ember;

export default Ember.Component.extend({
  notify: inject.service(),

  classNames: ['metadata-panel'],

  /**
   * A metadata record to edit in this panel.
   * To inject.
   * @type {ObjectProxy<FileProperty>}
   */
  metadataProxy: null,

  /**
   * To inject.
   * If true, metadata cannot be edited.
   * @type {Boolean}
   * @default
   */
  readOnly: false,

  isSaving: false,

  init() {
    this._super(...arguments);
  },

  metadata: computed.oneWay('metadataProxy.content'),

  metadataIsModified: computed('metadata.hasDirtyAttributes', function() {
    return this.get('metadata.hasDirtyAttributes');
  }),

  saveEnabled: computed('metadataIsModified', 'isSaving',
    function() {
      return this.get('metadataIsModified') && !this.get('isSaving');
    }
  ),

  removeAvailable: computed('metadata.isNew',
    function() {
      return !this.get('metadata.isNew');
    }
  ),

  removeEnabled: computed('isSaving',
    function() {
      return !this.get('isSaving');
    }
  ),

  didInsertElement() {
    this._super(...arguments);
    run.scheduleOnce('afterRender', this, function() {
      this.$().find('ul').addClass('nav-tabs');
    });
  },

  actions: {
    saveAll() {
      const i18n = this.get('i18n');
      this.set('isSaving', true);
      const p = this.get('metadata').save();
      p.then(() => {
        // TODO: file name - action should be sent down
        this.get('notify').info(i18n.t('components.metadataPanel.saveSuccess'));
      });

      p.catch((error) => {
        this.get('notify').error(i18n.t('components.metadataPanel.saveFailure', {
          errorMessage: error.message
        }));
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

    removeMetadata() {
      this.sendAction('removeMetadata');
    },
  }
});
