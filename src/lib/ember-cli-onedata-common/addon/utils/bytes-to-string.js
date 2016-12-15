/**
 * Util for converting number of bytes to size string. 
 *
 * @module utils/bytes-to-string
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

const TIB = 1099511627776;
const TERA = 1000000000000;
const GIB = 1073741824;
const GIGA = 1000000000;
const MIB = 1048576;
const MEGA = 1000000;
const KIB = 1024;
const KILO = 1000;

function bytesToStringIEC(bytes) {
  let number = bytes;
  let unit = 'B';
  if (bytes >= TIB) {
    unit = 'TiB';
    number = bytes/TIB;
  } else if (bytes >= GIB) {
    unit = 'GiB';
    number = bytes/GIB;
  } else if (bytes >= MIB) {
    unit = 'MiB';
    number = bytes/MIB;
  } else if (bytes >= KIB) {
    unit = 'KiB';
    number = bytes/KIB;
  }
  return [number, unit];
}

function bytesToStringSI(bytes) {
  let number = bytes;
  let unit = 'B';
  if (bytes >= TERA) {
    unit = 'TB';
    number = bytes/TERA;
  } else if (bytes >= GIGA) {
    unit = 'GB';
    number = bytes/GIGA;
  } else if (bytes >= MEGA) {
    unit = 'MB';
    number = bytes/MEGA;
  } else if (bytes >= KILO) {
    unit = 'KB';
    number = bytes/KILO;
  }
  return [number, unit];
}

/**
 * Convert number of bytes to human readable size string. Eg. 2.34 MB.
 * IEC format (KiB, MiB, etc.) can also be used (see options).
 * 
 * @param {Number} bytes
 * @param {Object} [options]
 * @param {Boolean} [options.iecFormat=false] If true, use IEC format: KiB, MiB, GiB
 */
export default function bytesToString(bytes, {iecFormat=false}={iecFormat:false}) {
  if (!bytes && bytes !== 0) {
    return '';
  } else {
    let [number, unit] =
      (iecFormat ? bytesToStringIEC : bytesToStringSI)(bytes);
    return `${Math.round(number * 10) / 10} ${unit}`;
  }
}
