import Ember from 'ember';
import ACE from 'op-worker-gui/utils/access-control-entity';
import safeExec from 'ember-cli-onedata-common/utils/safe-method-execution';

const {
  computed,
  on,
  observer,
  inject,
  run,
  get,
  RSVP: {
    Promise
  }
} = Ember;

export default Ember.Component.extend({
  store: inject.service(),
  fileSystemTree: inject.service(),

  /**
   * @virtual
   * Invoke this function to inform upper component about start editing
   * of ACL (ACE items will not be marked as not created after switching)
   * @type {Function}
   */
  startEdit: undefined,
  
  /**
   * @public
   * If true, ACE items will not be marked as not created after switching
   * permissions type
   * @type {boolean}
   */
  isEditing: false,
  
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
    } else if (this.get('isEditing')) {
      acl.forEach(ace => ace.set('isCreatedItem', false));
    }
    
    this.dataSpaceChanged();
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
      const modelName = `system${type.capitalize()}sModel`;
      let listModel = `${type}List`;
      let space = this.get('dataSpace');

      get(space, listModel)
        .then(list => get(list, `system${type.capitalize()}Records`))
        .then(systemRecords => safeExec(this, 'set', modelName, systemRecords))
        .catch(error => {
          safeExec(this, 'set', modelName, null);
          throw error;
        });
  },

  // -- try to fetch system users/groups list for selector

  dataSpaceChanged: observer('dataSpace', function() {
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
  }),

  // -- convert systemUsers/Groups RecordArrays to selectors elements

  systemUsers: computed('systemUsersModel.@each.{name,id}', function() {
    const models = this.get('systemUsersModel');
    if (models) {
      const selectData = models.map(m => ({
        id: m.get('id'),
        text: m.get('name')
      }));      
      return selectData;
    } else {
      return [];
    }
  }),

  systemGroups: computed('systemGroupsModel.@each.{name,id}', function() {
    const models = this.get('systemGroupsModel');
    if (models) {
      const selectData = models.map(m => ({
        id: m.get('id'),
        text: m.get('name')
      }));
      return selectData;
    } else {
      return [];
    }
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
