/**
 * Ember Class for using some ReplacingChunksArray as a record containing list
 * property
 *
 * @module utils/fake-list-record
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';
import FakeListHasMany from 'op-worker-gui/utils/fake-list-has-many';
import Ember from 'ember';

const {
  Object: EmberObject,
  RSVP: { Promise },
  computed,
  get,
} = Ember;

export default EmberObject.extend({
  /**
   * @virtual 
   * @type {ReplacingChunksArray}
   */
  initChunksArray: undefined,
  
  /**
   * @type {FakeListHasMany}
   */
  _listHasMany: undefined,
  
  list: computed(function () {
    const _chunksArray = this.get('_chunksArray');
    return PromiseObject.create({ promise: Promise.resolve(_chunksArray) });
  }),
  
  chunksArray: computed.reads('_chunksArray'),
  
  hasMany(relation) {
    if (relation === 'list') {
      return this.get('_listHasMany');
    } else {
      throw new Error('FakeListRecord: hasMany for non-"list" relation is not implemented');
    }
  },
  
  reload() {
    return this.get('_chunksArray')
      .reload(...arguments)
      .then(() => this);  
  },
  
  init() {
    this._super(...arguments);
    const _chunksArray = this.set('_chunksArray', this.get('initChunksArray'));
    this.set(
      '_listHasMany',
      new FakeListHasMany(get(_chunksArray, 'sourceArray'))
    );
  },
});