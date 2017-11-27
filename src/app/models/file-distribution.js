import Ember from 'ember';
import DS from 'ember-data';

const {
  computed,
  isEmpty,
} = Ember;

/**
 * Information about distribution of file blocks among single provider.
 * @module models/file-distribution
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  provider: DS.attr('string'),

  getProvider: computed(
    'provider',
    // TODO: context dependency
    
    function() {
      let store = this.get('store');
      return store.queryRecord('system-provider', {
        id: this.get('provider'),
        context: {}
      });
    }
  ),

  /**
    Array of integers. Number of elements should be even.
    - Let n=0,2,4,6,...
    - Each (n, n+1) pair of integers is a file block (start, end) bytes count.
    - Eg. [0,10,15,20,60,100] denotes, that there are 3 file blocks:
      - 0-10b, 15-20b, 60-100b
    - the file (which has a id = fileId) should have size >= last number (in this example >= 100b)
  */
  blocks: DS.attr({defaultValue: []}),
  
  file: DS.belongsTo('file', { async: true, inverse: null }),
    
  neverSynchronized: DS.attr('boolean', { defaultValue: false }),

  fileSize: computed.reads('file.size'),
  
  isEmpty: computed('fileSize', 'blocks.[]', function () {
    if (this.get('fileSize') !== undefined) {
      const blocks = this.get('blocks');
      return isEmpty(blocks) || compareNeighbors(blocks);
    }
  }),
  
  isComplete: computed('isEmpty', 'fileSize', 'blocks.[]', function () {
    const isEmpty = this.get('isEmpty');
    if (isEmpty === false) {
      const sblocks = this.get('blocks').map(i => parseInt(i)).sort((a, b) => a - b);
      return (
        sblocks[0] === 0 &&
        sblocks[sblocks.length - 1] === this.get('fileSize') &&
        compareNeighbors(sblocks.slice(1, sblocks.length - 1))
      );
    } else if (isEmpty === true) {
      return false;
    } else {
      return undefined;
    }
  }),
});

function compareNeighbors(sblocks) {
  for (let i = 0; i < sblocks.length; i += 2) {
    if (sblocks[i] !== sblocks[i+1]) {
      return false;
    }
  }
  return true;
}
