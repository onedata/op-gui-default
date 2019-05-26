/**
 * A hacky way to force reload the collection, eg. in `spaces-menu`
 * if some of records are not loaded (`!isLoaded`)
 *
 * @module mixins/force-reload-collection
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

 import Ember from 'ember';

const { 
  Mixin,
  get,
} = Ember;

export default Mixin.create({
  /**
   * To set in subclasses.
   * This collection will be reloaded in `forceReloadCollection` method
   * @type {DS.ManyArray}
   * @abstract
   */
  collection: null,
  
  /**
   * Time in milliseconds
   * @type {Number}
   */
  reloadCollectionTimeout: 750,
  
  /**
   * This is dirty hack to force reload of groups records after push record
   * It should be invoked after some events that might invoke a model push
   */
  forceReloadCollection() {
    let collection = this.get('collection');
    if (collection && collection.toArray().some(s => !get(s, 'isLoaded'))) {
      collection.reload();
    }
  },
  
  scheduleReloadCollection() {
    setTimeout(
      this.forceReloadCollection.bind(this),
      this.get('reloadCollectionTimeout')
    );
  },
});
