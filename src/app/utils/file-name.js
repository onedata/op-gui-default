/**
 * Parse POSIX file path and return file name
 * 
 * @param {string} path absolute file path (POSIX separators)
 * @returns {string} parsed file name
 */
export default function fileName(path) {
  if (path) {
    const matched = path.match(/.*\/(.*)/);
    return matched && matched[1];
  }
}
