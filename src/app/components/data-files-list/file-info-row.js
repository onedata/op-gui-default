import Ember from 'ember';

const {
  computed,
  Component,
  inject: { service },
} = Ember;

export default Component.extend({
  tagName: 'tr',
  classNames: ['first-level', 'file-info-row'],
  classNameBindings: ['highlightClass'],
  
  notify: service(),
  fileSystemTree: service(),

  /**
   * @virtual
   * File for which info is shown
   * @type {File}
   */
  file: null,
  
  /**
   * @virtual
   * @type {string}
   */
  spaceId: undefined,

  i18nPrefix: 'components.dataFilesList.fileInfoRow.',
  
  isLoading: computed.equal('metadataProxy.isPending', true),
  
  /**
   * Is true if failed to fetch file metadata
   * @type {Ember.ComputedProperty<boolean|undefined>}
   */
  metadataError: computed.reads('file.metadataError'),

  highlightClass: computed('file.isSelected', function() {
    return this.get('file.isSelected') ? 'active' : 'metadata-opened';
  }),

  /**
   * Handle remove result of metadata
   * @param {Boolean} [failed] if true, meta record destroy failed (it is not destroyed)
   * @param {Object} [error] error object, that should be present only of ``failed`` is true
   */
  handleMetadataRemoved(failed, error) {
    const fileName = this.get('file.name');
    const i18n = this.get('i18n');
    if (failed === true) {
      this.get('notify').error(
        i18n.t('components.dataFilesList.fileMetadataRow.metadataDeleteFailed', {
          fileName: fileName,
          errorMessage: error.message
        })
      );
    } else {
      this.get('notify').info(
        i18n.t('components.dataFilesList.fileMetadataRow.metadataDeleteSuccess', {
          fileName: fileName
        })
      );
      this.send('closeMetadataEditor');
    }
  },

  actions: {
    closeInfoViewer() {
      this.set('file.isShowingInfo', false);
    },
  }
});
