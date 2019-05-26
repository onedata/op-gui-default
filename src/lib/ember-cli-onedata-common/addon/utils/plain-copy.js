/**
 * NOTE: backported from `onedata-gui-common`
 * 
 * Creates JS plain object from given EmberObject
 *
 * Based on implementation: http://stackoverflow.com/a/15002755
 * NOTE that it will not clone object deeply, but just copy references
 *
 * @module utils/plain-copy
 * @author Jakub Liput, qrilka from Stack Overflow
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  isArray,
  get,
} = Ember;

// TODO: WARNING this function does not clone object deeply, 
// it will just return always a plain object

function emberObjPlainCopy(emberObj, deep = true) {
  var props = Object.keys(emberObj);
  var proto = Object.getPrototypeOf(emberObj);
  for (let p in proto) {
    if (proto.hasOwnProperty(p) && typeof (emberObj[p]) !== 'function') {
      props.push(p);
    }
  }
  var copy = {};
  props.forEach(function (p) {
    const value = get(emberObj, p);
    copy[p] = deep ? plainCopy(value) : value;
  }, emberObj);
  return copy;
}

export default function plainCopy(obj, deep = true) {
  if (isArray(obj)) {
    return deep ? obj.map(plainCopy) : obj;
  } else if (obj && (typeof obj === 'object')) {
    return emberObjPlainCopy(obj, deep);
  } else {
    return obj;
  }
}
