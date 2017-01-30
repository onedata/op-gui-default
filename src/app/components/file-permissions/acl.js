import Ember from 'ember';
import ACE from 'op-worker-gui/utils/access-control-entity';

const {
  computed,
  on,
  observer,
  inject,
  run,
  RSVP: {
    Promise
  }
} = Ember;

export default Ember.Component.extend({
  store: inject.service(),
  fileSystemTree: inject.service(),

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
    this._super(...arguments);
    let setMaxHeightFun = this.get('setMaxHeightFun');
    run.scheduleOnce('afterRender', this, function() {
      setMaxHeightFun();
      $(window).on('resize', this.get('setMaxHeightFun'));
    });
    
  },

  willDestroyElement() {
    this._super();
    $(window).off('resize', this.get('setMaxHeightFun'));
    this.set('modal.aclComponent', null);
  },

  /**
   * If true, loaded ACL is a sum of file ACLs.
   */
  mixedAcl: false,

  mixedAclChanged: on('init', observer('mixedAcl', function() {
    if (this.get('mixedAcl')) {
      this.setProperties({
        statusBlocked: false,
        // TODO: translate
        statusMessage: 'A presented ACL is constructed from many file ACLs. ' +
          'Please note that on submitting, all ACL of selected files will be replaced with ACL below.',
        statusType: 'warning'
      });
    }
  })),

  file: null,

  /**
   * Should be injected.
   * @type AccessControlEntity[]
   */
  acl: null,

  error: null,
  isLoadingModel: true,

  aclTmp: computed('acl.@each.subject', function() {
    return JSON.stringify(this.get('acl'));
  }),

  // TODO: change to be better synchronized with current file
  dataSpace: computed.alias('fileSystemTree.selectedSpace'),

  // -- we need these for displaying users/groups list for set permissions

  /**
   * @param {String} type - one of: user, group
   */
  fetchSystemModel(type) {
    const thisModel = `system${type.capitalize()}sModel`;
    let listModel = `${type}List`;
    let systemModel = `system${type.capitalize()}`;
    let space = this.get('dataSpace');

    let fetch = new Promise((resolve, reject) => {
      let getSpace = space.get(listModel);
      getSpace.then(list => {
        let getPermissions = list.get('permissions');
        getPermissions.then(permissions => {
          let getSystems = Promise.all(
            permissions.map(p => p.get(systemModel))
          );
          getSystems.then(resolve);
          getSystems.catch(reject);
        });
        getPermissions.catch(error => reject(error));
      });
      getSpace.catch(reject);
    });
    fetch.then(permissions => this.set(thisModel, permissions));
    fetch.catch(() => this.set(thisModel, null));
    return fetch;
  },

  // -- try to fetch system users/groups list for selector

  dataSpaceChanged: on('init', observer('dataSpace', function() {
    this.set('isLoadingModel', true);
    const promises = [
      this.fetchSystemModel('user'),
      this.fetchSystemModel('group')
    ];
    Promise.all(promises)
      .then(() => this.set('isLoadingModel', false))
      .catch(() => this.setProperties({
        statusBlocked: true,
        // TODO: translate
        statusMessage: 'List of available users or groups could not be loaded',
        statusType: 'error'
      }));
  })),

  // -- convert systemUsers/Groups RecordArrays to selectors elements

  systemUsers: computed('systemUsersModel.@each.{name,id}', function() {
    const models = this.get('systemUsersModel');
    let selectData = models.map(m => ({
      id: m.get('id'),
      text: m.get('name')
    }));
    return selectData;
  }),

  systemGroups: computed('systemGroupsModel.@each.{name,id}', function() {
    const models = this.get('systemGroupsModel');
    let selectData = models.map(m => ({
      id: m.get('id'),
      text: m.get('name')
    }));
    return selectData;
  }),

  setMaxHeightFun: computed(function() {
    return () => {
      $('#edit-permissions-modal .modal-dialog .modal-content .ace-items')
        .css('max-height', $(window).height() - 300);
    };
  }).readOnly(),

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
