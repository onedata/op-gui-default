/**
 * Check if some address can be reached using get image hack
 * 
 * @module utils/check-img
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  RSVP: { Promise },
} = Ember;
 
export default function checkImg(url) {
  const img = document.body.appendChild(document.createElement('img'));
  img.classList.add('hidden');
  return new Promise((resolve, reject) => {
    try {
      img.onload = () => resolve(true);
      img.onerror = () => resolve(false);
      img.src = url;
    } catch (error) {
      reject(error);
    }
  }).finally(() => document.body.removeChild(img));
}
