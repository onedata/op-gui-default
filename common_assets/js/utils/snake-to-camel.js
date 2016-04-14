// jshint esnext: true

/**
 * Converts some-string string to someString (snake case to camel case).
 * @module utils/snake-to-camel
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default function snakeToCamel(text, snakeSeparator) {
  let sep = snakeSeparator || '-';
  let re = new RegExp(`(\\${sep}\\w)`, 'g');
  return text.replace(re, (m) => { return m[1].toUpperCase(); });
}
