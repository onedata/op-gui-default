import Ember from 'ember';
import bindFloater from 'op-worker-gui/utils/bind-floater';

/**
 * Common functions for settings-drop components (space-settings-drop, group-settings-drop, etc.)
 * @module mixins/components/settings-drop
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create({
  classNames: ['item-element', 'item-icon'],

  // TODO: deregister event from sidebar on willDestroyElement
  // maybe use: this.on('willDestroyElement', () => { sidebar.off(...) } ) etc.
  didInsertElement() {
    let sidebar = $('.secondary-sidebar');
    let drop = this.$().find('.dropdown-menu');
    let updater = bindFloater(drop, null, {
      offsetX: 8
    });
    sidebar.on('scroll', updater);
    drop.on('mouseover', updater);

    // a hack to update drop position after space menu expand
    // this hack is probably not needed anymore, because spaces menu doesn't expand
    // on settings icon click - but we leave it "just in case"
    drop.closest('.settings-dropdown').on('click', function() {
      window.setTimeout(() => {
        updater();
      }, 50);
    });
  },
});
