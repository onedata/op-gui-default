import Ember from 'ember';
import SettingsDropMixin from 'op-worker-gui/mixins/components/settings-drop';

const {
  Component,
  computed,
  inject: { service },
} = Ember;

/**
 * Drop-right menu for root dir, containing ia. data distribution etc.
 * Component does not have spaces manipulation logic - actions are sent to parent components or services.
 * @module components/root-dir-settings-drop
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Component.extend(SettingsDropMixin, {
  classNames: ['root-dir-settings-drop'],

  commonModals: service(),
  messageBox: service(),

  dir: undefined,
  
  /**
    Items in "space settings" dropright menu
    Each item has properties:
    ```
    {
      icon: <string> - name of oneicon,
      label: <string> - label to show in menu (please use i18n service),
      action: <string> - name of action of this component
    }
    ```
  */
  menuItems: computed(function() {
    let i18n = this.get('i18n');
    return [
      {
        icon: 'provider',
        label: i18n.t('components.rootDirSettingsDrop.drop.dataDistribution'),
        action: 'dataDistribution'
      },
    ];
  }),

  actions: {
    dataDistribution() {
      this.sendAction('dataDistribution');
    },
  },
});
