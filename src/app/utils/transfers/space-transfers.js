import _ from 'lodash';

/**
 * @typedef {Object} SpaceTransfer
 * @property {string} providerId ID of transfer destination or source provider
 * @property {number} bytesPerSec recent total input or output transfer speed [B/s]
 */

export class SpaceTransfer {
  constructor(providerId, bytesPerSec) {
    this.providerId = providerId;
    this.bytesPerSec = bytesPerSec;
  }
}

export class SpaceInputTransfer extends SpaceTransfer {
  constructor(dest, bytesPerSec) {
    super(dest, bytesPerSec);
  }
  get dest() {
    return this.providerId;
  }
  set dest(dest) {
    this.providerId = dest;
  }
}

export class SpaceOutputTransfer extends SpaceTransfer {
  constructor(src, bytesPerSec) {
    super(src, bytesPerSec);
  }
  get src() {
    return this.providerId;
  }
  set src(src) {
    this.providerId = src;
  }
}
 
/**
 * @param {Array<ProviderTransfer>} providerTransfers
 * @param {string} [direction='output'] one of: input, output (or 'i', 'o' for short)
 * 
 * @export
 * @returns {Array<SpaceInputTransfer>}
 */
export default function spaceTransfers(providerTransfers, direction) {
  let groupProperty = 'src';
  let ResultClass = SpaceOutputTransfer;
  if (direction && _.startsWith(direction, 'i')) {
    groupProperty = 'dest';
    ResultClass = SpaceInputTransfer;
  }
  return _(providerTransfers)
    .groupBy(groupProperty)
    .map((itransfers, providerId) => {
      const totalBytesPerSec = _.sumBy(itransfers, 'bytesPerSec');
      return new ResultClass(providerId, totalBytesPerSec);
    })
    .value();
}
