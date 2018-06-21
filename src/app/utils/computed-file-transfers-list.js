/**
 * A computed property that creates FakeListRecordRelation with transfers list
 * filtered for given `fileId`.
 *
 * @module utils/computed-file-transfers-list
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
  get,
} = Ember;

const FileTransfersRCA = ReplacingChunksArray.extend({
  /**
   * @override
   * Last scheduled should be first on the list
   */
  sortFun(a, b) {
    const ai = get(a, 'scheduleTime');
    const bi = get(b, 'scheduleTime');
    if (ai > bi) {
      return -1;
    } else if (ai < bi) {
      return 1;
    } else {
      return 0;
    }
  },
});

export default function computedFileTransfersList() {
  return computed(function() {
    const fileId = this.get('id');
    const initChunksArray = FileTransfersRCA.create({
      fetch: (...args) => this.fetchTransfers(fileId, ...args),
      startIndex: 0,
      endIndex: 10000,
      indexMargin: 10,
    });
    return FakeListRecordRelation.create({ initChunksArray });
  });
}