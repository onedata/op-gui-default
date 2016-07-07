import Ember from 'ember';
import ACE from '../../utils/access-control-entity';

export default Ember.Component.extend({
  store: Ember.inject.service(),
  fileSystemTree: Ember.inject.service(),

  file: null,

  /**
   * Should be injected.
   * @type FileAcl
   */
  acl: null,

  // FIXME: move convertion to ace-item and convert each ACE/object separately?
  /**
   * Convert FileAcl.acl plain objects list <-> list of AccessControlEntity.
   * Stores object[] in acl.acl (attribute of target model).
   * - get converts: object[] -> AccessControlEntity[]
   * - set converts: AccessControlEntity[] -> object[]
   */
  aclItems: Ember.computed('acl.acl', {
    get(/*key*/) {
      return Ember.A(this.get('acl.acl').map(aceObject => ACE.create(aceObject)));
    },

    set(key, value) {
      return this.set('acl.acl', value.map(ace => ace.toJSON()));
    }
  }),

  aclTmp: function() {
    return JSON.stringify(this.get('acl.acl'));
  }.property('acl.acl'),

  // TODO: change to be better synchronized with current file
  dataSpace: Ember.computed.alias('fileSystemTree.selectedSpace'),

  // -- we need these for displaying users/groups list for set permissions

  fetchSystemUsersModel() {
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

  fetchSystemGroupsModel() {
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

  // -- observe system users/groups to update selectors when needed

  systemUsersChanged: function() {
    this.fetchSystemUsersModel();
  }.observes('dataSpace.space.userPermissions.@each.systemUser'),

  systemGroupsChanged: function() {
    this.fetchSystemGroupsModel();
  }.observes('dataSpace.space.groupPermissions.@each.systemGroup'),

  // -- convert systemUsers/Groups RecordArrays to selectors elements

  systemUsers: function() {
    const models = this.get('systemUsersModel');
    if (models && models.every(m => m && m.get('isLoaded'))) {
      return models.map(m => {
        return { id: m.get('id'), text: m.get('name') };
      });
    } else {
      return [];
    }
  }.property('systemUsersModel'),

  systemGroups: function() {
    const models = this.get('systemGroupsModel');
    if (models && models.every(m => m && m.get('isLoaded'))) {
      return models.map(m => {
        return { id: m.get('id'), text: m.get('name') };
      });
    } else {
      return [];
    }
  }.property('systemGroupsModel'),

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
    // addAc() {
    //   const r = this.get('store').createRecord('fileAcl', {
    //     file: this.get('file'),
    //     acl: [
    //       ACL.create().toJSON()
    //     ]
    //   });
    //   r.save().catch(() => {
    //     // debugger;
    //   });
    // }
  }
});
