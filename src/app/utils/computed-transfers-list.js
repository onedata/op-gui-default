/**
 * A computed property that creates FakeListRecordRelation with transfers list.
 * An object that uses this computed property should have implemented `fetchTransfers`
 * method.
 * The `type` is transfers type from backend.
 *
 * @module utils/computed-transfers-list
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 *
 * @function
 * @param {string} type one of: waiting, ongoing, ended
 */

import Ember from 'ember';
import ReplacingChunksArray from 'ember-cli-onedata-common/utils/replacing-chunks-array';
import FakeListRecordRelation from 'op-worker-gui/utils/fake-list-record-relation';

const {
  computed,
} = Ember;

export default function computedTransfersList(type) {
  return computed(function() {
    const initChunksArray = ReplacingChunksArray.create({
      fetch: (...args) => this.fetchTransfers(type, ...args),
      startIndex: 0,
      endIndex: 50,
      indexMargin: 10,
    });
    return FakeListRecordRelation.create({ initChunksArray });
  });
}


