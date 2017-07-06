/**
 * Utils for generating displayed ID conflicting names.
 *
 * @module utils/conflict-ids
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/**
 * Generate "minimal" resource IDs from full IDs
 * for naming conflicting files/spaces/groups etc.
 *  
 * @export
 * @param {String[]} ids
 * @returns {[String, String]} [minimal id for A, minimal id for B]
 */
export default function conflictIds(ids) {
  let maxLen = Math.max(...ids.map(name => name.length));

  let bufs = ids.map(name => ({name: name, buf: [], differs: false}));
  for (let i=0; i<maxLen; i+=1) {
    /* jshint loopfunc:true */ 
    let currentLetters = bufs.map(({name}) => name[i]);
    bufs.forEach(b => {
      if (!b.differs && currentLetters.filter(lt => lt === b.name[i]).length <= 1) {
        b.differs = true;
      }
    });

    bufs.forEach(b => b.buf.push(b.name[i]));

    if (bufs.every(b => b.differs) && i > 2) {
      break;
    }
  }

  return bufs.map(b => b.buf.join(''));
}
