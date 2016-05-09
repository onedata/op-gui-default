/**
 * Provides API for communication with Oneprovider Server.
 * @module services/oneprovider-server
 * @author Lukasz Opiola
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

export default Ember.Service.extend({
  server: Ember.inject.service('server'),

  /** TODO: should resolve space name on success */
  userJoinSpace(token) {
    return this.get('server').privateRPC('userJoinSpace', {token: token});
  },

  userLeaveSpace(space) {
    return this.get('server').privateRPC('userLeaveSpace', {
      spaceId: space.get('id')
    });
  },

  userLeaveGroup(group) {
    return this.get('server').privateRPC('userLeaveGroup', {
      groupId: group.get('id')
    });
  },

  /** Allowed types: user, group, support */
  getSpaceToken(type, spaceId) {
    if (type.match(/^(user|group|support)$/)) {
      this.getToken(type, {
        spaceId: spaceId
      });
    } else {
      throw `getSpaceToken type ${type} not supported`;
    }
  },

  /** Allowed types: user, group, createSpace */
  getGroupToken(type, groupId) {
    if (type.match(/^(user|group|createSpace)$/)) {
      this.getToken(type, {
        groupId: groupId
      });
    } else {
      throw `getGroupToken type ${type} not supported`;
    }
  },

  /** Generic function to fetch tokens */
  getToken(type, payload) {
    return this.get('server').privateRPC(`${type}Token`, payload);
  },

  fileUploadComplete(fileId) {
    return this.get('server').privateRPC('fileUploadComplete', {
      fileId: fileId
    });
  }
});
