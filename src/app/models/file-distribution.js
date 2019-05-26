import Ember from 'ember';
import DS from 'ember-data';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object'; 

const {
  computed,
} = Ember;

/**
 * Information about distribution of file blocks among single provider.
 * @module models/file-distribution
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  provider: DS.attr('string'),

  getProvider: computed(
    'provider',
    // TODO: context dependency
    
    function() {
      const store = this.get('store');
      const promise = store.queryRecord('system-provider', {
        id: this.get('provider'),
        context: {}
      });
      return PromiseObject.create({ promise });
    }
  ),
  
  /**
   * Used for drawing file distribution bar chart.
   * 
   * The format is an object, where keys are start pixel of bar (0-319)
   * and values are opacity of fill that should be used to fill the fragment
   * from start pixel to next start pixel.
   * 
   * Eg. `{ 0: 0, 160: 50, 300: 25 }` will draw:
   * - first half of the bar will be empty
   * - from the half of the bar, the bar will have 50% opacity
   * - on the end (300-319 pixels) the bar will have 25% opacity
   */
  chunksBarData: DS.attr('object', { defaultValue: { 0: 0 } }),
  
  /**
   * Float in range 0..100 with percentage of data blocks of this file on the provider
   */
  blocksPercentage: DS.attr('number', { defaultValue: 0 }),
  
  file: DS.belongsTo('file', { async: true, inverse: null }),
    
  neverSynchronized: DS.attr('boolean', { defaultValue: false }),

  fileSize: computed.reads('file.size'),
  
  isEmpty: computed('fileSize', 'chunksBarData', function () {
    return this.get('fileSize') !== undefined && !this.get('blocksPercentage');
  }),
  
  isComplete: computed('isEmpty', 'blocksPercentage', function () {
    const isEmpty = this.get('isEmpty');
    if (isEmpty === false) {
      return this.get('blocksPercentage') >= 100;
    } else if (isEmpty === true) {
      return false;
    } else {
      return undefined;
    }
  }),
});
