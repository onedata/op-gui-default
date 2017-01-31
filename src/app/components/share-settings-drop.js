import Ember from 'ember';
import SettingsDropMixin from 'op-worker-gui/mixins/components/settings-drop';

/**
 * Drop-right menu for single share, conaining i.a. rename action.
 * Component does not have shares manipulation logic - actions are sent to parent components or services.
 * @module components/space-settings-drop
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend(SettingsDropMixin, {
  commonModals: Ember.inject.service(),

  /**
    Items in dropright menu
    Each item has properties:
    ```
    {
      icon: <string> - name of oneicon,
      label: <string> - label to show in menu (please use i18n service),
      action: <string> - name of action of this component
    }
    ```
  */
  menuItems: function() {
    let i18n = this.get('i18n');
    return [
      {
        icon: 'rename',
        label: i18n.t('components.sharesMenu.drop.rename'),
        action: 'renameShare'
      },
      {
        icon: 'remove',
        label: i18n.t('components.sharesMenu.drop.remove'),
        action: 'removeShare'
      },
    ];
  }.property(),

  actions: {
    renameShare() {
      this.sendAction('openSettingsModal', 'rename', this.get('share'));
    },

    removeShare() {
      this.sendAction('openSettingsModal', 'remove', this.get('share'));
    },
  }
});
