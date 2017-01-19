import Ember from 'ember';

const {
  inject,
  computed,
  observer,
  isArray
} = Ember;

/**
 * Base for spaces/groups submenu options controllers - select submenu option on route's
 * model change.
 *
 * @module controllers/mixins/show-permission-controller
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default create;

/**
 * @param {string|string[]} permissionType user or group or [user, group]
 */
function create(permissionType) {
  let mixin = Ember.Mixin.create({
    secondaryMenu: inject.service(),

    subject: computed.alias('model'),

    canViewPermissions: computed.alias('subject.hasViewPrivilege'),

    changeMenuActiveOption() {
      let activeOption = Array.isArray(permissionType) ? 'members' : permissionType + 's';
      this.set('secondaryMenu.activeOption', activeOption);

      // TODO: control visible class of secondary sidebar with bound property
      // Ember.run.scheduleOnce('afterRender', this, function() {
      //   $('nav.secondary-sidebar').removeClass('visible');
      // });
    },

    onModelChange: observer('model', function () {
      this.changeMenuActiveOption();
    }),

  });

  let additionalAttributes = {};

  if (!isArray(permissionType)) {
    additionalAttributes['permissions'] = 
      computed.alias(`${permissionType}s-permissions`.camelize());

    permissionType = [permissionType];
  }

  // create aliases to permissions models (each is a ObjectProxy<RecordArray>)
  permissionType.forEach(pt => {
    additionalAttributes[`${pt}s-permissions`.camelize()] =
      computed.alias(`model.${pt}List.permissions`);
  });

  // names of created attributes
  let listAttributes =
      permissionType.map(pt => `model.${pt}List`);

  // one computed property to check if promises for fetching
  // all permissions list model are settled
  let listsAreSettled = listAttributes.map(li => li + '.isSettled');
  additionalAttributes['isLoadingList'] =
    computed(...listsAreSettled, function() {
      return listsAreSettled.any(s => !this.get(s));
    });
  
  additionalAttributes['isListRejected'] =
    computed(...listAttributes, 'isLoadingList', function() {
      if (this.get('isLoadingList')) {
        return false;
      } else {
        let listNotLoaded = listAttributes.every(la => {
          return this.get(la + '.content') == null;
        });
        return listNotLoaded;
      }
    });

  mixin.reopen(additionalAttributes);

  return mixin;
}
