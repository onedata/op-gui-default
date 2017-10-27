import DS from 'ember-data';
import Ember from 'ember';

import isDefaultMixinFactory from 'ember-cli-onedata-common/mixin-factories/models/is-default';

import PromiseArray from 'ember-cli-onedata-common/utils/ember/promise-array';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';

import _ from 'lodash';

const {
  attr,
  belongsTo
} = DS;

const {
  RSVP: { Promise },
  A,
} = Ember;

const ONE_MB = Math.pow(1024, 2);
const ONE_GB = Math.pow(1024, 3);
const ONE_TB = Math.pow(1024, 3);

// FIXME: mocked
const transferList = PromiseObject.create({ promise: Promise.resolve({
  list: PromiseArray.create({ promise: Promise.resolve(A([
    {
      destination: 'p2',
      fileName: 'file1',
      userName: 'John Smith',
      totalBytes: 3 * Math.pow(1024, 3),
      startedAt: new Date(),
      stats: {
        hour: {
          p1: _.range(60).map(i => i * ONE_MB),
          p3: _.range(60).map(i => i * 2 * ONE_MB),
        },
        day: {
          p1: _.range(24).map(i => i * ONE_GB),
          p3: _.range(24).map(i => i * 2 * ONE_GB),
        },
        month: {
          p1: _.range(30).map(i => i * ONE_TB),
          p3: _.range(30).map(i => i * 2 * ONE_TB),
        }
      }
    },
    {
      destination: 'p3',
      fileName: 'file2',
      userName: 'David Grohlton',
      totalBytes: 3 * Math.pow(1024, 3),
      startedAt: new Date(),
      stats: {
        hour: {
          p1: _.range(60).map(i => i * ONE_MB),
          p2: _.range(60).map(i => i * 2 * ONE_MB),
        },
        day: {
          p1: _.range(24).map(i => i * ONE_GB),
          p2: _.range(24).map(i => i * 2 * ONE_GB),
        },
        month: {
          p1: _.range(30).map(i => i * ONE_TB),
          p2: _.range(30).map(i => i * 2 * ONE_TB),
        }
      }
    },
  ]))}),
})});

const providerList = PromiseObject.create({ promise: Promise.resolve({
  list: PromiseArray.create({ promise: Promise.resolve(A([
    {
      id: 'p1',
      name: 'Provider One',
      latitude: 10,
      longitude: 20,
      status: 'online',
    },
    {
      id: 'p2',
      name: 'Provider Two',
      latitude: 40,
      longitude: 60,
      status: 'online',
    },
    {
      id: 'p3',
      name: 'Provider Three',
      latitude: -20,
      longitude: 30,
      status: 'online',
    },
    {
      id: 'p4',
      name: 'Provider Four',
      latitude: 50,
      longitude: -50,
      status: 'pending',
    },
  ]))}),
})});

/**
 * A configuration of a space - entry point for all options
 * that can be reached from "spaces" button in primary sidebar.
 *
 * @module models/space
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(isDefaultMixinFactory('defaultSpaceId'), {
  /** User specified name of space that will be exposed in GUI */
  name: attr('string'),

  hasViewPrivilege: attr('boolean'),

  /*** RELATIONS */

  user: belongsTo('user', { async: true }),

  /** A root directory with space files. It must be a dir-type File! */
  rootDir: belongsTo('file', { async: true }),

  /** Collection of users permissions - effectively all rows in permissions table */
  userList: belongsTo('space-user-list', { async: true }),
  
  /** Collection of group permissions - effectively all rows in permissions table */
  groupList: belongsTo('space-group-list', { async: true }),

  // FIXME: currently mocked, to uncomment
  // transferList: belongsTo('space-transfer-list', { async: true, inverse: null }),
  // providerList: belongsTo('space-provider-list', { async: true, inverse: null }),
  
  transferList,
  providerList,
});
