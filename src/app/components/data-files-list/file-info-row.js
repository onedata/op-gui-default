/**
 * Additional row in files list to show information about above file
 * 
 * @module components/data-files/list/file-info-row
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

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
  
  highlightClass: computed('file.isSelected', function highlightClass() {
    return this.get('file.isSelected') ? 'active' : 'info-opened panel-opened';
  }),

  actions: {
    closeInfoViewer() {
      this.set('file.isShowingInfo', false);
    },
  }
});
