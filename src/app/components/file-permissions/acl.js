import Ember from 'ember';
import ACL from '../../utils/access-control-entity';

export default Ember.Component.extend({
  store: Ember.inject.service(),
  fileSystemTree: Ember.inject.service(),

  file: null,

  /**
   * Should be injected.
   * @type FileAccessList[]
   */
  // acl: Ember.computed.alias('file.fileAcl'),

  // acl: null,

  // TODO: change to be better synchronized with current file
  dataSpace: Ember.computed.alias('fileSystemTree.selectedSpace'),

  fetchSystemUsersModel: function() {
    this.get('dataSpace.space.userPermissions').then(ups => {
      const suPromises = ups.map(up => up.get('systemUser'));
      const allSuPromise = Ember.RSVP.Promise.all(suPromises);
      allSuPromise.then(suList => {
        this.set('systemUsersModel', suList);
      });
      allSuPromise.catch(error => {
        console.warn(`Error on getting systemUsers for ACL: ${error}`);
        this.set('systemUsersModel', []);
      });
    });
  },

  usersPermissionsChanged: function() {
    this.fetchSystemUsersModel();
  }.observes('dataSpace.space.userPermissions.@each.name'),
  // }.observes('dataSpace', 'dataSpace.space', 'dataSpace.space.userPermissions',
    // 'dataSpace.space.userPermissions.[]', 'dataSpace.space.userPermissions.@each.name'),

  fetchSystemGroupsModel: function() {
    this.get('dataSpace.space.groupPermissions').then(gps => {
      const sgPromises = gps.map(gp => gp.get('systemGroup'));
      const allSgPromise = Ember.RSVP.Promise.all(sgPromises);
      allSgPromise.then(sgList => {
        this.set('systemGroupsModel', sgList);
      });
      allSgPromise.catch(error => {
        console.warn(`Error on getting systemGroups for ACL: ${error}`);
        this.set('systemGroupsModel', []);
      });
    });
  },

  groupsPermissionsChanged: function() {
    this.fetchSystemGroupsModel();
  }.observes('dataSpace.space.groupPermissions.@each.name'),
  // }.observes('dataSpace', 'dataSpace.space', 'dataSpace.space.groupPermissions',
  //   'dataSpace.space.groupPermissions.[]', 'dataSpace.space.groupPermissions.@each.name'),

  systemUsers: function() {
    const sum = this.get('systemUsersModel');
    if (sum) {
      return sum.map(su => {
        return {
          id: su.get('id'),
          text: su.get('name')
        };
      });
    } else {
      return [];
    }
  }.property('systemUsersModel'),

  systemGroups: function() {
    const sum = this.get('systemGroupsModel');
    if (sum) {
      return sum.map(su => {
        return {
          id: su.get('id'),
          text: su.get('name')
        };
      });
    } else {
      return [];
    }
  }.property('systemGroupsModel'),

  initTriggered: function() {
    this.get('store').find('file-acl', this.get('file.id')).then(
      (acl) => {
        // debugger;
        this.set('acl', acl);
      },
      (error) => {
        console.warn('ACL for file cannot be fetched: ' + error.message);
      }
    );
  }.on('init'),

  didInsertElement() {
    this.fetchSystemUsersModel();
    this.fetchSystemGroupsModel();
  },

  // FIXME: test code, using fake ACL
  // acl: this.get('store').createRecord('fileAcl', {
  //   file: this.get('file'),
  //   acl: [
  //     ACL.create({
  //       type: 'deny',
  //       subject: 'everyone',
  //       permissions: 2
  //     }),
  //     ACL.create({
  //       type: 'allow',
  //       subject: 'owner',
  //       permissions: 4
  //     }),
  //   ]
  // }),

  actions: {
    // TODO: just for tests
    addAc() {
      const r = this.get('store').createRecord('fileAcl', {
        file: this.get('file'),
        acl: [
          ACL.create().toJSON()
        ]
      });
      r.save().catch(() => {
        // debugger;
      });
    }
  }
});
