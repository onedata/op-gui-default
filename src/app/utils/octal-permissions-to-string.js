let separatePerms = function(perms) {
  let splitted = [];
  splitted.push(Math.floor(perms / 100));
  splitted.push(Math.floor((perms / 10) % 10));
  splitted.push(Math.floor(perms % 10));
  return splitted;
};

/**
 * Convert POSIX permissions written in octal to stringified, like in "ls".
 * @module utils/octal-permissions-to-string
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/// Some POSIX octal permissions help
// 0 -> no
// 1 -> execute
// 2 -> write
// 3 -> write & execute
// 4 -> read
// 5 -> read & execute
// 6 -> read write
// 7 -> rwx
let singleOctalToString = function(octal) {
  let exec = ((octal & 1) === 1);
  let write = ((octal & 2) === 2);
  let read = ((octal & 4) === 4);

  return `${read ? 'r' : '-'}${write ? 'w' : '-'}${exec ? 'x' : '-'}`;
};


/**
 * Returns a strigified POSIX permissions.
 *
 * @param {integer} octal POSIX permissions in octal format, eg. 664
 * @returns {string} POSIX persmissions string as in "ls" command, eg. "rw-rw-r--"
 */
export default function octalPermissionsToString(octalPerms) {
  return separatePerms(octalPerms).map((octal) => singleOctalToString(octal)).join('');
}
