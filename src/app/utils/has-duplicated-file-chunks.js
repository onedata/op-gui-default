/**
 * Returns true if some of chunks defined in `subjectChunks` exists in any array
 * of chunks defined in `compareChunksArray`.
 * 
 * @module utils/has-duplicated-file-chunks
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 * 
 * @param {Array<number>} subjectChunks chunks which are going to be checked
 * @param {Array<Array<number>>} compareChunksArray array of chunks that is
 * a source of possible chunk duplication
 * @returns {boolean}
 */
export default function hasDuplicatedFileChunks(subjectChunks, compareChunksArray) {
  const compareChunksSum = sumChunks(compareChunksArray);
  
  let sumSearchIndex = 0;
  for (let i = 0; i < subjectChunks.length; i += 2) {
    const chunkStart = subjectChunks[i];
    const chunkEnd = subjectChunks[i + 1];
    while (compareChunksSum.length > sumSearchIndex &&
      compareChunksSum[sumSearchIndex + 1] < chunkStart) {
      sumSearchIndex += 2;
    }
    if (compareChunksSum.length > sumSearchIndex &&
      compareChunksSum[sumSearchIndex] <= chunkEnd) {
      return true;
    }
  }
  return false;
}

/**
 * Calculates sum of all chunks available in chunksArray
 * @param {Array<Array<number>>} chunksArray 
 * @returns {Array<number>}
 */
function sumChunks(chunksArray) {
  let sum = [];
  chunksArray.forEach(chunks => {
    let newSum = [0, 0], i = 0, j = 0;
    while (i < sum.length || j < chunks.length) {
      let getFromSum;
      if (i >= sum.length) {
        getFromSum = false;
      } else if (j >= chunks.length) {
        getFromSum = true;
      } else {
        getFromSum = sum[i] <= chunks[j];
      }

      const minStartChunks = getFromSum ? sum : chunks;
      const minStartIndex = getFromSum ? i : j;
      const newSumMax = newSum[newSum.length - 1];
      if (minStartChunks[minStartIndex] <= newSumMax) {
        if (minStartChunks[minStartIndex + 1] > newSumMax) {
          newSum[newSum.length - 1] = minStartChunks[minStartIndex + 1];
        }
      } else {
        newSum.push(minStartChunks[minStartIndex], minStartChunks[minStartIndex + 1]);
      }
      if (getFromSum) {
        i += 2;
      } else {
        j += 2;
      }
    }
    if (newSum[0] === 0 && newSum[1] === 0) {
      newSum = newSum.slice(2);
    }
    sum = newSum;
  });
  return sum;
}
