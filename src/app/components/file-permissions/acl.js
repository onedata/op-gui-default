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
  fileAcl: null,

  aclTmp: function() {
    return JSON.stringify(this.get('fileAcl.acl'));
  }.property('fileAcl.acl.@each.subject'),

  // TODO: change to be better synchronized with current file
  dataSpace: Ember.computed.alias('fileSystemTree.selectedSpace'),

  // -- we need these for displaying users/groups list for set permissions

  /**
   * @param {String} type - one of: user, group
   */
  fetchSystemModel(type) {
    const permModel = `${type}Permissions`;
    const systemModel = `system${type.capitalize()}`;
    const thisModel = `system${type.capitalize()}sModel`;

    if (this.get('dataSpace')) {
      this.get('dataSpace.space')
        .then(space => {
          space.get(permModel).then(ups => {
            const suPromises = ups.map(up => up.get(systemModel));
            const allSuPromise = Ember.RSVP.Promise.all(suPromises);
            allSuPromise.then(suList => {
              this.set(thisModel, suList);
            });
            allSuPromise.catch(error => {
              console.warn(`Error on getting system ${type}s for ACL: ${error.message}`);
              this.set('systemUsersModel', null);
            });
          });
        })
        .catch(error => {
          console.warn(`Error on getting space for ACL: ${error.message}`);
          this.set('systemUsersModel', null);
        });
    }
  },

  // -- try to fetch system users/groups list for selector

  dataSpaceChanged: function() {
    this.fetchSystemModel('user');
    this.fetchSystemModel('group');
  }.observes('dataSpace').on('init'),

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

  setMaxHeightFun: function() {
    return () => {
      $('#edit-permissions-modal .modal-dialog .modal-content .ace-items')
        .css('max-height', $(window).height() - 300);
    };
  }.property().readOnly(),

  didInsertElement() {
    this.get('setMaxHeightFun')();
    $(window).on('resize', this.get('setMaxHeightFun'));
  },

  willDestroyElement() {
    $(window).off('resize', this.get('setMaxHeightFun'));
  },

  actions: {
    removeAceItem(ace) {
      const acl = this.get('fileAcl.acl');
      acl.removeObject(ace);
    },

    moveUp(ace) {
      const acl = this.get('fileAcl.acl');
      const index = acl.indexOf(ace);
      if (index > 0) {
        const tmp = acl.objectAt(index-1);
        acl.replace(index-1, 1, acl.objectAt(index));
        acl.replace(index, 1, tmp);
      }
    },

    moveDown(ace) {
      const acl = this.get('fileAcl.acl');
      const index = acl.indexOf(ace);
      if (index < acl.length-1) {
        const tmp = acl.objectAt(index+1);
        acl.replace(index+1, 1, acl.objectAt(index));
        acl.replace(index, 1, tmp);
      }
    },

    createAce() {
      this.get('fileAcl.acl').pushObject(ACE.create());
    }
  }
});
