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

  /**
   * Use an invitation token to join a space for which the token was generated.
   *
   * @param {String} token A token generated with ``this.getTokenUserJoinSpace``
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(spaceName)`` when successfully joined to space
   * - ``reject(error)`` on failure
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
   * - ``reject(error)`` on failure
   */
  userLeaveSpace(spaceId) {
    return this.get('server').privateRPC('userLeaveSpace', {
      spaceId: spaceId
    });
  },

  /**
   * The current user (session user) leaves a group.
   *
   * @param {String} groupId An ID of the group to leave
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve()`` when successfully left the space
   * - ``reject(error)`` on failure
   */
  userLeaveGroup(groupId) {
    return this.get('server').privateRPC('userLeaveGroup', {
      groupId: groupId
    });
  },

  /**
   * Fetch an invitation to group token.
   * Pass the token to user which wants to join the group.
   *
   * @param {String} groupId An ID of the group to invite
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(token)`` when successfully fetched the token (string)
   * - ``reject(error)`` on failure
   */
  getTokenUserJoinGroup(groupId) {
    return this.get('server').privateRPC('getTokenUserJoin', {
      groupId: groupId
    });
  },

  /**
   * Fetch an invitation to space token.
   * Pass the token to user which wants to join the space.
   *
   * @param {String} spaceId An ID of the space to invite
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(token)`` when successfully fetched the token (string)
   * - ``reject(error)`` on failure
   */
  getTokenUserJoinSpace(spaceId) {
    return this.get('server').privateRPC('getTokenUserJoin', {
      spaceId: spaceId
    });
  },

  /**
   * Fetch an invitation to group token.
   * Pass the token to group which wants to join the space.
   *
   * @param {String} groupId An ID of the group to invite
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(token)`` when successfully fetched the token (string)
   * - ``reject(error)`` on failure
   */
  getTokenGroupJoinGroup(groupId) {
    return this.get('server').privateRPC('getTokenGroupJoin', {
      groupId: groupId
    });
  },

  /**
   * Fetch an invitation to space token.
   * Pass the token to group which wants to join the space.
   *
   * @param {String} groupId An ID of the group to invite
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(token)`` when successfully fetched the token (string)
   * - ``reject(error)`` on failure
   */
  getTokenGroupJoinSpace(spaceId) {
    return this.get('server').privateRPC('getTokenGroupJoin', {
      spaceId: spaceId
    });
  },

  /**
   * Fetch a token, which can be passed to a provider to support the specified space.
   *
   * @param {String} spaceId An ID of the space to invite
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(token)`` when successfully fetched the token (string)
   * - ``reject(error)`` on failure
   */
  getTokenProviderSupport(spaceId) {
    return this.get('server').privateRPC('getTokenProviderSupport', {
      spaceId: spaceId
    });
  },

  /**
   * Fetch a token, which can be passed to a provider which can support
   * the newly created space.
   *
   * @param {String} groupId An ID of the group, which will use a newly created space
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(token)`` when successfully fetched the token (string)
   * - ``reject(error)`` on failure
   */
  getTokenRequestSpaceCreation(groupId) {
    return this.get('server').privateRPC('getTokenRequestSpaceCreation', {
      groupId: groupId
    });
  },

  /**
   * Use an invitation token to join a group into a space for which the token was generated.
   *
   * @param {String} groupId An ID of the group, which will use a newly created space
   * @param {String} token A token generated with ``this.getTokenUserJoinSpace``
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve(token)`` when successfully fetched the token (string)
   * - ``reject(error)`` on failure
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
   * - ``reject(error)`` on failure
   */
  groupLeaveSpace(groupId, spaceId) {
    return this.get('server').privateRPC('groupLeaveSpace', {
      groupId: groupId,
      spaceId: spaceId
    });
  },

  /**
   * Notify the backend, that file upload has been completed (no more chunks to send)
   *
   * @param {String} fileId An ID of file, which upload has been completed.
   * See {@link service/file-upload} to find out more about file IDs.
   * @returns {RSVP.Promise} A backend operation completion:
   * - ``resolve()`` - always after completion, no reject in this method
   */
  fileUploadComplete(fileId) {
    return this.get('server').privateRPC('fileUploadComplete', {
      fileId: fileId
    });
  }
});
