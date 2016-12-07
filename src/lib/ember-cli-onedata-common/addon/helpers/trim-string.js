import Ember from 'ember';

/**
 * Cut a string to be max of ``length`` length.
 * If cutted, overwrite last 3 letters (13, 14, 15) of shortened string.
 * @returns {String}
 */
export function trimString([text, length]) {
  if (text) {
    length = length || 15;

    let addEllipsis = (text.length > length);

    return addEllipsis ? (text.substring(0, length-3) + '...') : text;
  } else {
    return text;
  }
}

export default Ember.Helper.helper(trimString);
