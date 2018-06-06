/**
 * Additional abstraction layer for using `FakeListRecord` like a model relation
 *
 * @module utils/fake-list-record-relation
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';
import FakeListRecord from 'op-worker-gui/utils/fake-list-record';
import Ember from 'ember';

const {
  computed,
  get,
} = Ember;

export default PromiseObject.extend({
  isLoading: computed.reads('isPending'),
  isLoaded: computed.not('isLoading'),
  reload() {
    return this.get('_fakeListRecord').reload(...arguments);
  },
  init() {
    this._super(...arguments);
    const _fakeListRecord = this.set('_fakeListRecord', FakeListRecord.create({
      initChunksArray: this.get('initChunksArray'),
    }));
    this.set(
      'promise',
      get(_fakeListRecord, 'chunksArray.initialLoad')
        .then(() => _fakeListRecord)
    );
  },
});
