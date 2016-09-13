/**
 * Provides API for communication with Oneprovider Server.
 * Every API method returns a RVSP.Promise, which passes the error object on reject.
 * The error object have always a ``message`` property which is a string with a error description.
 * See methods documentation for information about parameters and the promise's ``resolve`` arguments.
 * @module services/oneprovider-server
 * @author Lukasz Opiola
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Service.extend({
  server: Ember.inject.service('server'),

  // FIXME: only for Share testing purposes
  store: Ember.inject.service(),

  /**--------------------------------------------------------------------
   File upload related procedures
   -------------------------------------------------------------------- */
  /**
   * Notify the backend, that file upload has been completed
   * successfully (no more chunks to send)
   *
   * @param {String} uploadId An ID of file, which upload has been completed.
   * See {@link service/file-upload} to find out more about file IDs.
   * @param {String} connectionRef An ID session
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve()`` - always after completion, no reject in this method
   */
  fileUploadSuccess(uploadId, connectionRef) {
    return this.get('server').privateRPC('fileUploadSuccess', {
      uploadId: uploadId,
      connectionRef: connectionRef
    });
  },

  /**
   * Notify the backend, that file upload failed (no more chunks will be sent)
   *
   * @param {String} uploadId An ID of file, which upload has been completed.
   * See {@link service/file-upload} to find out more about file IDs.
   * @param {String} connectionRef An ID session
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve()`` - always after completion, no reject in this method
   */
  fileUploadFailure(uploadId, connectionRef) {
    return this.get('server').privateRPC('fileUploadFailure', {
      uploadId: uploadId,
      connectionRef: connectionRef
    });
  },

  /**--------------------------------------------------------------------
   File download related procedures
   -------------------------------------------------------------------- */

  /**
   * Fetch direct URL for file download.
   *
   * @param {String} fileId An ID of file to download
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when file can be downloaded
   *  - ``data.fileUrl`` (string) - URL for file GET (using session auth)
   * - ``reject(object: error)`` on failure or when file cannot be downloaded
   */
  getFileDownloadUrl(fileId) {
    return this.get('server').privateRPC('getFileDownloadUrl', {
      fileId: fileId
    });
  },

  /**--------------------------------------------------------------------
   Files/directories operations
   -------------------------------------------------------------------- */

  /**
   * Request creation of a Share for given directory.
   *
   * @param {String} fileId An ID of the directory that will be shared with a Share
   * @param {String} shareName Name of new Share
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully created the share
   *   - ``data.shareId`` (string) - an ID of the created Share record
   * - ``reject(object: error)`` on failure
   */
  createFileShare(fileId, shareName) {
    // TODO: implement in backend
    return this.get('server').privateRPC('createFileShare', {
      fileId: fileId,
      shareName: shareName,
    });

    // FIXME: mocked share creation
    // return new Ember.RSVP.Promise((resolve, reject) => {
    //   const store = this.get('store');
    //   store.findRecord('file', fileId).then(
    //     (file) => {
    //       const shareRecord = this.get('store').createRecord('share', {
    //         id: file.get('id'),
    //         name: shareName,
    //         file: file,
    //         // WARNING - a test code, lack of dataSpace:...
    //         publicUrl: 'https://example.com/'+dataSpaceId
    //       });
    //       file.setProperties({
    //         share: shareRecord,
    //       });
    //       file.save().then(
    //         () => {
    //           resolve({shareId: shareRecord.get('id')});
    //         },
    //         () => {
    //           reject({message: 'save File/Share failed'});
    //         }
    //       );
    //     },
    //     () => reject({message: 'no file for file id'})
    //   );
    // });
  },

  /**--------------------------------------------------------------------
   Space related procedures
   -------------------------------------------------------------------- */
  /**
   * Fetch an invitation to space token.
   * Pass the token to user which wants to join the space.
   *
   * @param {String} spaceId An ID of the space to invite
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully fetched the token
   *   - ``data.token`` (string) - a token
   * - ``reject(object: error)`` on failure
   */
  getTokenUserJoinSpace(spaceId) {
    return this.get('server').privateRPC('getTokenUserJoinSpace', {
      spaceId: spaceId
    });
  },

  /**
   * Use an invitation token to join a space for which the token was generated.
   *
   * @param {String} token A token generated with ``this.getTokenUserJoinSpace``
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully joined to space
   *   - ``data.spaceName`` - name of space that user joined to
   * - ``reject(object: error)`` on failure
   */
  userJoinSpace(token) {
    return this.get('server').privateRPC('userJoinSpace', {
      token: token
    });
  },

  /**
   * The current user (session user) leaves a space.
   *
   * @param {String} spaceId An ID of the space to leave
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve()`` when successfully left the space
   * - ``reject(object: error)`` on failure
   */
  userLeaveSpace(spaceId) {
    return this.get('server').privateRPC('userLeaveSpace', {
      spaceId: spaceId
    });
  },

  /**
   * Fetch an invitation to space token.
   * Pass the token to group which wants to join the space.
   *
   * @param {String} spaceId An ID of the space to which a group is invited
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully fetched the token
   *   - ``data.token`` a token
   * - ``reject(object: error)`` on failure
   */
  getTokenGroupJoinSpace(spaceId) {
    return this.get('server').privateRPC('getTokenGroupJoinSpace', {
      spaceId: spaceId
    });
  },

  /**
   * Use an invitation token to join a group into a space for which the token was generated.
   *
   * @param {String} groupId An ID of the group, which will use a newly created space
   * @param {String} token A token generated with ``this.getTokenUserJoinSpace``
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully fetched the token
   *   - ``data.spaceName`` - a name of space that group joined to
   * - ``reject(object: error)`` on failure
   */
  groupJoinSpace(groupId, token) {
    return this.get('server').privateRPC('groupJoinSpace', {
      groupId: groupId,
      token: token
    });
  },

  /**
   * Remove group from a space
   *
   * @param {String} groupId An ID of the group which will be removed from space
   * @param {String} spaceId An ID of the space to leave
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve()`` on success
   * - ``reject(object: error)`` on failure
   */
  groupLeaveSpace(groupId, spaceId) {
    return this.get('server').privateRPC('groupLeaveSpace', {
      groupId: groupId,
      spaceId: spaceId
    });
  },

  /**
   * Fetch a token, which can be passed to a provider to support the specified space.
   *
   * @param {String} spaceId An ID of the space to invite
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully fetched the token
   *   - ``data.token`` a token
   * - ``reject(object: error)`` on failure
   */
  getTokenProviderSupport(spaceId) {
    return this.get('server').privateRPC('getTokenProviderSupportSpace', {
      spaceId: spaceId
    });
  },

  /**--------------------------------------------------------------------
   Group related procedures
   -------------------------------------------------------------------- */
  /**
   * Fetch an invitation to group token.
   * Pass the token to user which wants to join the group.
   *
   * @param {String} groupId An ID of the group to invite
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully fetched the token
   *   - ``data.token`` a token
   * - ``reject(object: error)`` on failure
   */
  getTokenUserJoinGroup(groupId) {
    return this.get('server').privateRPC('getTokenUserJoinGroup', {
      groupId: groupId
    });
  },

  /**
   * Use an invitation token to join a group for which the token was generated.
   *
   * @param {String} token A token generated with ``this.getTokenUserJoinGroup``
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully joined to group
   *   - ``data.groupName`` - a name of group that user joined to
   * - ``reject(object: error)`` on failure
   */
  userJoinGroup(token) {
    return this.get('server').privateRPC('userJoinGroup', {
      token: token
    });
  },

  /**
   * The current user (session user) leaves a group.
   *
   * @param {String} groupId An ID of the group to leave
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve()`` when successfully left the space
   * - ``reject(object: error)`` on failure
   */
  userLeaveGroup(groupId) {
    return this.get('server').privateRPC('userLeaveGroup', {
      groupId: groupId
    });
  },

  /**
   * Fetch an invitation to group token.
   * Pass the token to group which wants to join the space.
   *
   * @param {String} groupId An ID of the group to invite
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully fetched the token
   *   - ``data.token`` a token
   * - ``reject(object: error)`` on failure
   */
  getTokenGroupJoinGroup(groupId) {
    return this.get('server').privateRPC('getTokenGroupJoinGroup', {
      groupId: groupId
    });
  },

  /**
   * Use a group invitation token to join a group for which the token was generated.
   *
   * @param {String} groupId An ID of the group which will be added as a subgroup to other group
   * @param {String} token A token generated with ``this.getTokenUserJoinGroup``
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully fetched the token
   *   - ``data.groupName`` a token
   * - ``reject(object: error)`` on failure
   */
  groupJoinGroup(groupId, token) {
    return this.get('server').privateRPC('groupJoinGroup', {
      groupId: groupId,
      token: token
    });
  },

  /**
   * The chosen group leaves a group.
   *
   * @param {String} parentGroupId An ID of the group from which childGroup will leave
   * @param {String} childGroupId An ID of the group which should leave from parentGroup
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve()`` when successfully left group
   * - ``reject(object: error)`` on failure
   */
  groupLeaveGroup(parentGroupId, childGroupId) {
    return this.get('server').privateRPC('groupLeaveGroup', {
      parentGroupId: parentGroupId,
      childGroupId: childGroupId
    });
  },

  /**
   * Fetch a token, which can be passed to a provider which can support
   * the newly created space.
   *
   * @param {String} groupId An ID of the group, which will use a newly created space
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully fetched the token
   *   - ``data.token`` a token
   * - ``reject(object: error)`` on failure
   */
  getTokenRequestSpaceCreation(groupId) {
    return this.get('server').privateRPC('getTokenRequestSpaceCreation', {
      groupId: groupId
    });
  }
});
