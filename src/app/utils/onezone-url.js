/**
 * Construct absolute path to some Onezone view on current origin
 * 
 * @module utils/onezone-url
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

export default function onezoneUrl(path = '') {
  return `/ozw/onezone/i#/${path}`;
}
