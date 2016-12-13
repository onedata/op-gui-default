/**
 * A Component to show collections of changeable checkboxes with permissions
 * for each "permission" entity. The entity can be e.g. a userPermission record.
 * Effectively, on desktop this should be a table, where each row respresents
 * single permission collection for entity.
 *
 * @module components/permissions-table
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/

import Ember from 'ember';

export default Ember.Component.extend({
  oneproviderServer: Ember.inject.service(),
  commonModals: Ember.inject.service(),
  notify: Ember.inject.service(),
  store: Ember.inject.service(),

  classNames: ['permissions-table'],

  /**
    An object which owns the permissions property - this can be a User or Group.
    It is used to identify the subject of table action, eg. invite user.
  */
  subject: null,

  // FIXME: overlay on true
  isLocked: false,

  /** Unfortunately, some colors are used by spin.js and must be passed from JS code
    should be the same as $onedata-gray **/
  colorSpinnerDisabled: '#B6BAC0',

  /** Common size of buttons spinners */
  spinnerSize: 18,
  /** Common size of spin-buttons */
  spinButtonSize: 's',

  // these colors are currently not used - but stay here just in case
  // // should be the same as $onedata-green-save
  // colorSpinnerSave: '#4FD56E',
  // // should be the same as $onedata-red-discard
  // colorSpinnerDiscard: '#F14549',

  permissionsSorting: ['owner'],

  /**
   * Collection of permissions-base model subclasses instances.
   * Each represents a sigle entity with some permissions to set.
   *
   * It must be injected into component.
   */
  usersPermissions: null,
  usersPermissionsSorted: Ember.computed.sort('usersPermissions', 'permissionsSorting'),

  groupsPermissions: null,
  groupsPermissionsSorted: Ember.computed.sort('groupsPermissions', 'permissionsSorting'),

  availableGroups: function() {
    if (this.get('groupsPermissions')) {
      return this.get('store').findAll('group');
    } else {
      return null;
    }
  }.property('groupsPermissions'),

  /**
   * For what entity permissions are? space/group
   * It must be injected into component.
   */
  subjectType: null,

  typeSingular: function() {
    let type = this.get('type');
    return (type.slice(-1) === 's') ? type.slice(0, -1) : type;
  }.property('type'),

  inviteButton: function() {
    switch (this.get('type')) {
      case 'users':
        return 'user-add';
      case 'groups':
        return 'group-invite';
      default:
        return null;
    }
  }.property('type'),

  /** Should permissions table be treated as modified and not saved?
   *  It is true when at least one permission model in collection is modified.
   */
  isModified: function() {
    let up = this.get('usersPermissions');
    let gp = this.get('groupsPermissions');
    let upModified = up ? up.any(p => p.get('isModified')) : false;
    let gpModified = gp ? gp.any(p => p.get('isModified')) : false;
    return upModified || gpModified;
  }.property('usersPermissions.@each.isModified', 'groupsPermissions.@each.isModified'),

  isModifiedChanged: function() {
    this.sendAction('modifiedChanged', this.get('isModified'), this.get('subjectType'));
  }.observes('isModified'),

  activePermissions: null,

  allPermissions: function() {
    return [].concat(
      this.get('usersPermissions') && this.get('usersPermissions').toArray() || []
    ).concat(
      this.get('groupsPermissions') && this.get('groupsPermissions').toArray() || []
    );
  }.property('usersPermissions', 'groupsPermissions'),

  actions: {
    /** Change state of single permission checkbox */
    togglePermission: function(permission, propertyName) {
      var permName = 'perm' + propertyName;
      var modName = 'mod' + propertyName;
      permission.set(permName, !permission.get(permName));
      permission.set(modName, !permission.get(modName));
    },

    /** Save all permission models in table */
    saveChanges: function() {
      this.set('isLocked', true);
      let promises = this.get('allPermissions').map((permission) => {
        if (permission.get('isModified')) {
          return permission.save().then(
            () => {
              console.debug('permission ' + permission + ' saved successfully');
              permission.setUnmodified();
            },
            (error) => {
              error = (error && error.message) || this.get('i18n').t('common.unknownError');
              console.debug('permission ' + permission + ' saving failed: ' + error);
              this.get('notify').error(this.get('i18n')
              .t('components.permissionsTable.notify.saveFailedSingle', {
                name: permission.get('owner.name')
              }) + ': ' + error);
            }
          );
        }
      });
      let masterPromise = Ember.RSVP.Promise.all(promises);
      masterPromise.finally(() => this.set('isLocked', false));
      masterPromise.catch((error) => {
        error = error || this.get('i18n').t('common.unknownError');
        this.get('notify').error(
          `${this.get('i18n').t('components.permissionsTable.notify.saveFailedAny')}: ${error}`
        );
      });
      return masterPromise;
    },

    /** Bring back all permission models from table to state before user modification */
    discardChanges: function() {
      this.get('allPermissions').forEach(function(permission) {
        permission.reset();
      });
      return new Ember.RSVP.Promise((resolve) => {
        resolve();
      });
    },

    inviteUserToSpace() {
      this.get('commonModals').openModal(`token` , {
        type: `userJoinSpace`,
        funArgs: [this.get('subject.id')]
      });
    },

    inviteGroupToSpace() {
      this.get('commonModals').openModal(`token` , {
        type: `groupJoinSpace`,
        funArgs: [this.get('subject.id')]
      });
    },

    inviteUserToGroup() {
      this.get('commonModals').openModal(`token` , {
        type: `userJoinGroup`,
        funArgs: [this.get('subject.id')]
      });
    },

    inviteGroupToGroup() {
      this.get('commonModals').openModal(`token` , {
        type: `groupJoinGroup`,
        funArgs: [this.get('subject.id')]
      });
    },

    activatePermissions(permissions) {
      if (this.get('activePermissions.id') === permissions.get('id')) {
        this.set('activePermissions', null);
      } else {
        this.set('activePermissions', permissions);
      }
    }
  }
});
