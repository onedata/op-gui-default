/**
 * Abstraction layer that mocks HasMany relationship using some SliceArray
 *
 * @module utils/fake-list-has-many
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

export default class FakeListHasMany {
  /**
   * @param {Ember.Array} sourceArray
   */
  constructor(sourceArray) {
    this.sourceArray = sourceArray;
  }
  ids() {
    return this.sourceArray.mapBy('id').toArray();
  }
}
