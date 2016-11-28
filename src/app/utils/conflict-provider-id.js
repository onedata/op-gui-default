/**
 * Utils for generating displayed ID of provider in conflicting files.
 *
 * @module utils/conflict-provider-id
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/**
 * Generate 2 "minimal" provider IDs from 2 full provider IDs
 * for naming conflicting files.
 *  
 * @export
 * @param {String[]} names
 * @returns {[String, String]} [minimal id for A, minimal id for B]
 */
export default function conflictProviderId(providerNames) {
  let maxLen = Math.max(...providerNames.map(name => name.length));

  let bufs = providerNames.map(name => ({name: name, buf: [], differs: false}));
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
