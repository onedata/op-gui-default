import Ember from 'ember';

const {
  inject: {
    service
  },
  computed,
  computed: {
    alias
  },
  observer
} = Ember;

/**
 * Loads list of spaces for further data browsing.
 * 
 * @module controllers/onedata/data
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Controller.extend({
  i18n: service(),
  fileBrowser: service(),
  commonLoader: service(),

  spaces: alias('model'),

  isLoadingSpaces: computed('spaces.isLoaded', 'spaces.@each.isLoaded', function() {
    return this.get('spaces.isLoaded') !== true || this.get('spaces').any((s) => !s.get('isLoaded'));
  }),

  isLoadingChanged: observer('isLoadingSpaces', function() {
    let {
      commonLoader, isLoadingSpaces
    } = this.getProperties('commonLoader', 'isLoadingSpaces');
    if (isLoadingSpaces) {
      let i18n = this.get('i18n');
      commonLoader.setProperties({
        isLoading: true,
        message: i18n.t('components.commonLoader.synchronizingSpaces'),
        messageSecondary: i18n.t('components.commonLoader.firstLogin'),
        area: 'main-content'
      });
    } else {
      commonLoader.setProperties({
        isLoading: false,
        message: null,
        messageSecondary: null,
      });
    }
  }),
});
