/**
 * Modifies collection of passed `orig` array to be the same as `update` array.
 * 
 * NOTE: extended from `onedata-gui-common` `util:push-new-items`
 * 
 * @module utils/mutate-array
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import _ from 'lodash';
import emberObjectMerge from 'ember-cli-onedata-common/utils/ember-object-merge';

function equal(x, y) {
  return x === y;
}

export default function mutateArray(orig, update, compare = equal, deep = true) {
  const isEmberArray = !!orig.pushObject;
  let arrayModified = false;
  try {
    const pushFun = isEmberArray ? 'pushObject' : 'push';
     
    const toRemove = [];
    for (let i = 0; i < orig.length; i++) {
      /* jshint loopfunc: true */
      const uitem = orig[i];
      const eindex = _.findIndex(update, oitem => compare(oitem, uitem));
      if (eindex === -1) {
        toRemove.push(i);
      }
    }
    for (let i = toRemove.length - 1; i >= 0; i--) {
      orig.splice(toRemove[i], 1);
    }
    if (!_.isEmpty(toRemove)) {
      arrayModified = true;
    }
    
    _.forEach(update, uitem => {
      const matchItemIndex = _.findIndex(orig, oitem => compare(oitem, uitem));
      if (matchItemIndex >= 0) {
        emberObjectMerge(orig[matchItemIndex], uitem, deep);
        arrayModified = true;
      } else {
        orig[pushFun](uitem);
      }
    });
  } finally {
    if (isEmberArray && arrayModified) {
      orig.arrayContentDidChange();
    }
  }
  return orig;
}


