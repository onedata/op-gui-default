/**
 * Create a new pathsDir from other pathsDir, with new dir root.
 * @module utils/cut-dirs-path
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/**
 * Cut dirs in path to create new root dir.
 *
 * @param {File[]} dirsPath array of File objects represeting:
 *  original root dir > dir_1 > ... > file/dir_N
 * @param {File} rootDir one of dir_1 ... dir_N, that will become new root dir
 *  in result
 * @returns {File[]} dirsPath with new root
 */
export default function cutDirsPath(dirsPath, rootDir) {
  const i = dirsPath.map(d => d.get('id')).indexOf(rootDir.get('id'));
  return (i > -1) ? dirsPath.slice(i) : dirsPath;
}
