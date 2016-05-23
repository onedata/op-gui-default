import ENV from '../config/environment';

export default {
  name: 'testing-model',
  after: 'store',
  initialize: function(container/*, application*/) {
    if (ENV.environment !== 'development-localstorage') {
      console.debug(`Skipping testing-model creation because environment is not development-localstorage`);
    } else {
      let store = container.lookup('service:store');

      /// spaces

      let space1 = store.createRecord('space', {
        id: 1,
        name: 'My space',
        isDefault: false
      });


      let space2 = store.createRecord('space', {
        id: 2,
        name: 'Second space',
        isDefault: true
      });

      let systemUser1 = store.createRecord('system-user', {
        name: 'Jakub Liput'
      });

      let systemUser2 = store.createRecord('system-user', {
        name: 'Łukasz Opioła'
      });

      let space1up1 = store.createRecord('space-user-permission', {
        owner: systemUser1,
        permViewSpace: true,
        permModifySpace: false,
        permRemoveSpace: false,
        permInviteUser: false,
        permRemoveUser: false,
        permInviteGroup: false,
        permRemoveGroup: false,
        permSetPrivileges: false,
        permInviteProvider: false,
        permRemoveProvider: false,
      });

      let space1up2 = store.createRecord('space-user-permission', {
        owner: systemUser2,
        permViewSpace: true,
        permModifySpace: true,
        permRemoveSpace: true,
        permInviteUser: true,
        permRemoveUser: true,
        permInviteGroup: true,
        permRemoveGroup: true,
        permSetPrivileges: true,
        permInviteProvider: true,
        permRemoveProvider: true,
      });

      let space2up1 = store.createRecord('space-user-permission', {
        owner: systemUser1,
        permViewSpace: true,
        permModifySpace: false,
        permRemoveSpace: true,
        permInviteUser: false,
        permRemoveUser: true,
        permInviteGroup: false,
        permRemoveGroup: true,
        permSetPrivileges: false,
        permInviteProvider: true,
        permRemoveProvider: false,
      });


      space1.get('userPermissions').pushObject(space1up1);
      space1.get('userPermissions').pushObject(space1up2);

      space2.get('userPermissions').pushObject(space2up1);

      /// groups

      let group1 = store.createRecord('group', {
        id: 1,
        name: 'First group',
        isDefault: false
      });

      let group2 = store.createRecord('group', {
        id: 2,
        name: 'Second group',
        isDefault: true
      });

      let systemGroup2 = store.createRecord('system-group', {
        id: 1,
        // we use the same name as in Group to indicate it represents the same group...
        name: 'Second group',
        isDefault: true
      });

      let group1up1 = store.createRecord('group-user-permission', {
        owner: systemUser1,
        permViewGroup: true,
        permModifyGroup: false,
        permSetPrivileges: false,
        permRemoveGroup: false,
        permInviteUser: false,
        permRemoveUser: false,
        permCreateSpace: false,
        permJoinSpace: false,
        permLeaveSpace: false,
        permGetSupport: false,
      });

      let group1up2 = store.createRecord('group-user-permission', {
        owner: systemUser2,
        permViewSpace: true,
        permModifySpace: true,
        permRemoveSpace: true,
        permInviteUser: true,
        permRemoveUser: true,
        permInviteGroup: true,
        permRemoveGroup: true,
        permSetPrivileges: true,
        permInviteProvider: true,
        permRemoveProvider: true,
      });

      let group1gp1 = store.createRecord('group-group-permission', {
        owner: systemGroup2,
        permViewSpace: true,
        permModifySpace: true,
        permRemoveSpace: true,
        permInviteUser: true,
        permRemoveUser: true,
        permInviteGroup: true,
        permRemoveGroup: true,
        permSetPrivileges: true,
        permInviteProvider: true,
        permRemoveProvider: true,
      });

      group1.get('userPermissions').pushObject(group1up1);
      group1.get('userPermissions').pushObject(group1up2);

      // set parent group of group2
      group2.get('parentGroups').pushObject(group1);
      group1.get('groupPermissions').pushObject(group1gp1);


      /// save all records
      space1.save();
      space2.save();
      systemUser1.save();
      systemUser2.save();
      space1up1.save();
      space1up2.save();
      space2up1.save();
      group1.save();
      group2.save();
      systemGroup2.save();
      group1up1.save();
      group1up2.save();
      group1gp1.save();
    }
  }
};
