import Ember from 'ember';

const {
  Mixin,
  RSVP: {
    Promise
  }
} = Ember;

const DEFAULT_OPTIONS = {
  nonEmpty: false
}

/**
 * Create a Mixin for onedata subroute that uses one of User's collection.
 * @function create
 * @param collectionName {String} name of User's hasMany relation
 * @param options.nonEmpty {Boolean} if true, fetched collection cannot be empty
 *                                   and if it is, it will reject the model
 */
export default function create(collectionName, { nonEmpty=false } = DEFAULT_OPTIONS) {
  // create 2 variants of collection resolve/reject functions:
  // 1. if it must be non-empty, check if it's non-empty
  // 2. always resolve otherwise
  let resolveCollection;
  if (nonEmpty) {
    resolveCollection = function (collection, resolve, reject) {
      if (collection && collection.get('length') > 0) {
        resolve(collection);
      } else {
        console.error(`util:user-collection-model: User collection ${collectionName} is empty but it shouldn't be`);
        reject({
          type: 'empty'
        });
      }
    };
  } else {
    resolveCollection = function (collection, resolve) {
      resolve(collection);
    };
  }
  
  return Mixin.create({
    model() {
      return new Promise((resolve, reject) => {
      let collectionFetching = this.modelFor('onedata').get(collectionName);
      collectionFetching.then(collection => resolveCollection(collection, resolve, reject));
      collectionFetching.catch(error => reject({
        type: 'fetchError',
        error: error
      }));
    });
    }
  });
}
