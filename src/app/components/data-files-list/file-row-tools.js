/**
 * Additional icon buttons for file row
 * 
 * @module components/data-file-list/file-row-tools
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  computed,
  Component,
  inject: { service },
} = Ember;

export default Component.extend({
  fileSystemTree: service(),

  classNames: ['file-row-tools'],
  classNameBindings: ['highlightClass'],

  /**
   * To inject.
   * A file that this tool operate on.
   * @type {File}
   */
  file: null,

  /**
   * To inject.
   * If true, tools for file manipulation are disabled.
   * @type {Boolean}
   * @default
   */
  readOnly: false,

  metadataClass: computed('file.isEditingMetadata', 'file.hasMetadata', 'readOnly', function() {
    if (this.get('file.isEditingMetadata')) {
      return 'active';
    } else if (this.get('file.hasMetadata')) {
      return 'visible-on-parent-hover-25p';
    } else if (this.get('readOnly')) {
      return 'hidden';
    } else {
      return 'visible-on-parent-hover';
    }
  }),
  
  infoClass: computed('file.isShowingInfo', function infoClass() {
    return this.get('file.isShowingInfo') ? 'active' : 'visible-on-parent-hover';
  }),

  actions: {
    shareFile() {
      this.sendAction('shareFile', this.get('file'));
    },
    metadataClicked() {
      if (this.get('file.hasMetadata')) {
        this.send('toggleFileMetadata');
      } else if (!this.get('readOnly')) {
        this.send('createFileMetadata');
      }
    },
    toggleFileMetadata() {
      this.sendAction('toggleFileMetadata', this.get('file'));
    },
    toggleFileInfo() {
      this.sendAction('toggleFileInfo', this.get('file'));
    },
    createFileMetadata() {
      const file = this.get('file');
      this.get('fileSystemTree').toggleMetadataEditor(file);
    }
  }
});
