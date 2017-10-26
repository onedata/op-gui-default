/**
 * Reduce multiple arrays into one by summing values on the same positions
 * Eg.: `[[1,2,3], [10,20,30], [100,200,300]]` -> `[111, 222, 333]`
 * 
 * @param {Array<Array<number>>} arrays 
 * @returns {Array<number>}
 */
export default function reduceArrays(...arrays) {
  const arrayCount = arrays.length;
  const arrayLength = arrays[0].length;
  const buf = [];
  for (let pos = 0; pos < arrayCount; pos += 1) {
    let posSum = 0;
    for (let arrayNum = 0; arrayNum < arrayLength; arrayNum += 1) {
      posSum += (arrays[arrayNum][pos] || 0);
    }
    buf.push(posSum);
  }
  return buf;
}
