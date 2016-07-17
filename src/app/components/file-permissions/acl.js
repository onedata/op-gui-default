import Ember from 'ember';
import ACE from '../../utils/access-control-entity';

export default Ember.Component.extend({
  store: Ember.inject.service(),
  fileSystemTree: Ember.inject.service(),

  init() {
    this._super();
    this.set('modal.aclComponent', this);

    // injected ACL items should be considered as non-new
    const acl = this.get('acl');

    if (!acl) {
      this.set(
        'error',
        this.get('i18n').t('components.filePermissions.acl.errorCannotLoadACL')
      );
    } else {
      acl.forEach(ace => ace.set('isCreatedItem', false));
    }
  },

  didInsertElement() {
    this.get('setMaxHeightFun')();
    $(window).on('resize', this.get('setMaxHeightFun'));
  },

  willDestroyElement() {
    this._super();
    $(window).off('resize', this.get('setMaxHeightFun'));
    this.set('modal.aclComponent', null);
  },

  file: null,

  /**
   * Should be injected.
   * @type AccessControlEntity[]
   */
  acl: null,

  error: null,
  isLoadingModel: true,

  aclTmp: function() {
    return JSON.stringify(this.get('acl'));
  }.property('acl.@each.subject'),

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

    return new Ember.RSVP.Promise((resolve, reject) => {
      if (this.get('dataSpace')) {
        this.get('dataSpace.space')
          .then(space => {
            space.get(permModel).then(ups => {
              const suPromises = ups.map(up => up.get(systemModel));
              const allSuPromise = Ember.RSVP.Promise.all(suPromises);
              allSuPromise.then(suList => {
                this.set(thisModel, suList);
                resolve();
              });
              allSuPromise.catch(error => {
                console.warn(`Error on getting system ${type}s for ACL: ${error.message}`);
                this.set(thisModel, null);
                reject();
              });
            });
          })
          .catch(error => {
            console.error(`Error on getting space for ACL: ${error.message}`);
            this.set(thisModel, null);
            reject();
          });
      }
    });
  },

  // -- try to fetch system users/groups list for selector

  dataSpaceChanged: function() {
    this.set('isLoadingModel', true);
    const promises = [
      this.fetchSystemModel('user'),
      this.fetchSystemModel('group')
    ];
    Ember.RSVP.Promise.all(promises)
      .then(() => this.set('isLoadingModel', false))
      // TODO: translate
      .catch(() => this.set('error', 'Users or groups data could not be loaded'));
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

  /**
   * ACL is valid if all of its elements are valid.
   */
  isValid: function() {
    return this.get('acl').every(ace => ace.get('isValid'));
  }.property('acl.@each.isValid'),

  isReadyToSubmit: function() {
    return !this.get('error') && !this.get('isLoadingModel') &&
      this.get('isValid');
  }.property('error', 'isLoadingModel', 'isValid'),

  actions: {
    removeAceItem(ace) {
      const acl = this.get('acl');
      acl.removeObject(ace);
    },

    moveUp(ace) {
      const acl = this.get('acl');
      const index = acl.indexOf(ace);
      if (index > 0) {
        const tmp = acl.objectAt(index-1);
        acl.replace(index-1, 1, acl.objectAt(index));
        acl.replace(index, 1, tmp);
      }
    },

    moveDown(ace) {
      const acl = this.get('acl');
      const index = acl.indexOf(ace);
      if (index < acl.length-1) {
        const tmp = acl.objectAt(index+1);
        acl.replace(index+1, 1, acl.objectAt(index));
        acl.replace(index, 1, tmp);
      }
    },

    createAce() {
      this.get('acl').pushObject(ACE.create({
        isCreatedItem: true
      }));
    }
  }
});
