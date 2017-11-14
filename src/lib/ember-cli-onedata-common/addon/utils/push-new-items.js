/**
 * NOTE: backported and extended from `onedata-gui-common`
 * 
 * Adds new items to array without changing original array reference and
 * reference to items that are deeply equal to these in new array.
 *
 * @module utils/merge-new-items
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import _ from 'lodash';
import emberObjectMerge from 'ember-cli-onedata-common/utils/ember-object-merge';

export default function mergeNewItems(orig, update, compare, deep = true) {
  const isEmberArray = !!orig.pushObject;
  const pushFun = isEmberArray ? 'pushObject' : 'push';

  _.forEach(update, uitem => {
    const matchItemIndex = _.findIndex(orig, oitem => compare(oitem, uitem));
    if (matchItemIndex >= 0) {
      emberObjectMerge(orig[matchItemIndex], uitem, deep);
      if (isEmberArray) {
        orig.arrayContentDidChange();
      }
    } else {
      orig[pushFun](uitem);
    }
  });
  return orig;
}


