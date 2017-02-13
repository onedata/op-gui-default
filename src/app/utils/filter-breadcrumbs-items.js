import Ember from 'ember';
import addEllipsisBreadcrumbsItem from './add-ellipsis-breadcrumbs-item';

/**
 * A function for reducing number of breadcrumbs items (for long paths).
 * @module utils/filter-breadcrumbs-items
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/**
 * @function
 * Filters elements of given breadcrumbs items array
 * to get array with reduced number of breadcrumbs items.
 * 
 * The filter chooses items using priority algorithm for adding items to result,
 * preservig their order from original array. The priority is:
 * 1. add last item (current dir),
 * 2. add root item,
 * 3. add parent of last item,
 * 4. add first child of root item,
 * 5. add parents of parent of last item, starting from last parent.
 * 
 * Also an "ellispis item" presented as "..." is added, which is a link to parent
 * of right item on its right.
 * 
 * Example: we got items: ``root > a > b > c > d > e > f > current_dir``.
 * Invoking ``filterBreadcrumbsItems(items, 5)`` willl give us:
 * ``root > a > ... > e > f > current_dir`` where "..." is "ellipsis item".
 * 
 * @param {Ember.A<FileBreadcrumbsItem>} items
 * @param {Number} count max. number of dir names from ``items`` that should be included
 *                       in result array
 * @returns {RSVP.Promise<Ember.A<FileBreadcrumbsItem>>} resolves with reduced breadcrumbs
 *                                         items array;
 *                                         max. lenght of the array is ``count+1``
 *                                         or ``items`` length
 */
function filterBreadcrumbsItems(items, count) {
  let resultArray = Ember.A();
  let itemsCount = items.get('length');
  // at least 1
  if (count > 0 && itemsCount > 0) {
    // add last element (current dir)
    // [current_dir]
    resultArray.push(items.get('lastObject'));
  } else {
    // return empty array
    // []
    return new Ember.RSVP.Promise(resolve => resolve(resultArray));
  }
  // 2 or more: []
  if (count > 1 && itemsCount > 1) {
    // add root item at the front of items
    // [root > pwd]
    resultArray.splice(0, 0, items.get('firstObject'));
  } else {
    // only one element, but add ellipsis item if can
    // [... > pwd]
    return addEllipsisBreadcrumbsItem(resultArray, resultArray.get('firstObject'));
  }
  // 3 or more
  if (count > 2 && itemsCount > 2) {
    // add parent of current dir before current dir
    // [root > pwd_parent > pwd]
    resultArray.splice(1, 0, items.objectAt(items.length-2));
  } else {
    // [root > ... > pwd]
    return addEllipsisBreadcrumbsItem(resultArray, resultArray.get('lastObject'));
  }
  // 4 or more
  if (count > 3 && itemsCount > 3) {
    // add first child of root
    // [root > root_child > pwd_parent > pwd]
    resultArray.splice(1, 0, items.objectAt(1));
  } else {
    // [root > ... > pwd_parent > pwd]
    return addEllipsisBreadcrumbsItem(resultArray, resultArray.objectAt(1));
  }
  // 5 or more
  if (count > 4 && itemsCount >= 4) {
    let lastItemToAddIndex = itemsCount - 2;
    let firstItemToAddIndex = lastItemToAddIndex - (count - 4);
    // first item should not be lower than first child of root
    // because we already added it earlier
    firstItemToAddIndex = Math.max(2, firstItemToAddIndex);
    let frontArray = resultArray.slice(0, 2);
    let middleItems = items.slice(firstItemToAddIndex, lastItemToAddIndex);
    let tailArray = resultArray.slice(2, 5);   

    resultArray = Ember.A(
      frontArray.concat(middleItems, tailArray)
    );
  }

  return addEllipsisBreadcrumbsItem(resultArray, resultArray.objectAt(2));
}

export default filterBreadcrumbsItems;
