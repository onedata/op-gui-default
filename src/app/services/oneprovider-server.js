/**
 * Provides API for communication with Oneprovider Server.
 * Every API method returns a RVSP.Promise, which passes the error object on reject.
 * The error object have always a ``message`` property which is a string with a error description.
 * See methods documentation for information about parameters and the promise's ``resolve`` arguments.
 * @module services/oneprovider-server
 * @author Lukasz Opiola
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Service.extend({
  server: Ember.inject.service('server'),

  /**--------------------------------------------------------------------
   File upload related procedures
   -------------------------------------------------------------------- */
  /**
   * Notify the backend, that file upload has been completed
   * successfully (no more chunks to send)
   *
   * @param {String} uploadId An ID of file, which upload has been completed.
   * See {@link service/file-upload} to find out more about file IDs.
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve()`` - always after completion, no reject in this method
   */
  fileUploadSuccess(uploadId, parentId) {
    return this.get('server').privateRPC('fileUploadSuccess', {
      uploadId: uploadId,
      parentId: parentId,
    });
  },

  /**
   * Notify the backend, that file upload failed (no more chunks will be sent)
   *
   * @param {String} uploadId An ID of file, which upload has been completed.
   * See {@link service/file-upload} to find out more about file IDs.
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve()`` - always after completion, no reject in this method
   */
  fileUploadFailure(uploadId, parentId) {
    return this.get('server').privateRPC('fileUploadFailure', {
      uploadId: uploadId,
      parentId: parentId,
    });
  },

  /**
   * Notify the backend, that batch file upload has been completed.
   * It means, that we can close upload progress for directory.
   *
   * @param {String} parentId An ID of directory (File), to which upload has
   * been completed.
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` - always after completion, no reject in this method
   *  - ``data.newChildrenCount`` - (number) new count of cached children files
   *    in parent directory. Note, that this is not total number of files in parent.
   */
  fileBatchUploadComplete(parentId) {
    return this.get('server').privateRPC('fileBatchUploadComplete', {
      parentId: parentId
    });
  },


  /**--------------------------------------------------------------------
   File download related procedures
   -------------------------------------------------------------------- */

  /**
   * Fetch direct URL for file download - authenticated user.
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

  /**
   * Fetch direct URL for file download - authenticated user in shared view.
   *
   * @param {String} fileId An ID of file to download
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when file can be downloaded
   *  - ``data.fileUrl`` (string) - URL for file GET (using session auth)
   * - ``reject(object: error)`` on failure or when file cannot be downloaded
   */
  getSharedFileDownloadUrl(fileId) {
    return this.get('server').privateRPC('getSharedFileDownloadUrl', {
      fileId: fileId
    });
  },

  /**
   * Fetch direct URL for file download - public mode.
   *
   * @param {String} fileId An ID of file to download
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when file can be downloaded
   *  - ``data.fileUrl`` (string) - URL for file GET (using session auth)
   * - ``reject(object: error)`` on failure or when file cannot be downloaded
   */
  getPublicFileDownloadUrl(fileId) {
    return this.get('server').publicRPC('getPublicFileDownloadUrl', {
      fileId: fileId
    });
  },

  /**--------------------------------------------------------------------
   Files/directories operations
   -------------------------------------------------------------------- */

   /**
    * Request creation of a File for given parent directory.
    *
    * @param {String} fileName Name of new File
    * @param {String} parentId ID of File record that is a parent dir for new file
    * @param {String} type Type-string of file object: "file" or "dir"
    * @returns {RSVP.Promise} A backend operation completion:
    * - ``resolve(object: data)`` when successfully created the share
    *   - ``data.fileId`` (string) - an ID of the created File record
    * - ``reject(object: error)`` on failure
    */
  createFile(fileName, parentId, type) {
    return this.get('server').privateRPC('createFile', {
      fileName: fileName,
      parentId: parentId,
      type: type,
    });
  },

  /**
   * Request backend to send more children files for given directory.
   * Used for dynamically load more files for directory.
   *
   * @param {String} dirId ID of File record which is a directory whose
   *                       children should be loaded
   * @param {Number} currentChildrenCount Current number of loaded directory
   *                                      children
   * @param {String} fileModelType can be: file, file-shared, file-public
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(object: data)`` when successfully created the share
   *   - ``data.newChildrenCount`` (number) - new number of children of directory
   * - ``reject(object: error)`` on failure
   */
  fetchMoreDirChildren(dirId, currentChildrenCount, fileModelType) {
    let methodScope = (fileModelType === 'file-public' ? 'public' : 'private');

    return this.get('server')[methodScope + 'RPC']('fetchMoreDirChildren', {
      dirId: dirId,
      currentChildrenCount: currentChildrenCount,
      fileModelType: fileModelType,
    });
  },

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
    return this.get('server').privateRPC('createFileShare', {
      fileId: fileId,
      shareName: shareName,
    });
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

  /**--------------------------------------------------------------------
   Transfer related procedures
   -------------------------------------------------------------------- */
  
  /**
   * Cancels the transfer
   * 
   * @param {string} transferId 
   * @returns {RSVP.Promise} A backend operation completion,
   * ``resolve()`` when cancelling procedure has started successfully
   * ``reject(object: error)`` on failure
   */
  cancelTransfer(transferId) {
    return this.get('server').privateRPC('cancelTransfer', {
      transferId,
    });
  },

  /**
   * Reruns the transfer
   * 
   * @param {string} transferId 
   * @returns {RSVP.Promise} A backend operation completion,
   * ``resolve()`` when rerunning procedure has started successfully
   * ``reject(object: error)`` on failure
   */
  rerunTransfer(transferId) {
    return this.get('server').privateRPC('rerunTransfer', {
      transferId,
    });
  },
});
