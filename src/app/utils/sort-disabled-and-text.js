/**
 * Sort function for sorting items in selector: `disabled` items goes bottom,
 * and all items are sorted by `text`.
 * 
 * @module utils/sort-disabled-and-text
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  get,
} = Ember;

export default function sortDisabledAndText(a, b) {
  const aEnabled = !get(a, 'disabled');
  const bEnabled = !get(b, 'disabled');
  if (aEnabled && !bEnabled) {
    return -1;
  } else if (aEnabled && bEnabled || !aEnabled && !bEnabled) {
    const aText = get(a, 'text');
    const bText = get(b, 'text');
    return aText.localeCompare(bText, undefined, { sensitivity: 'base' });
  } else {
    return 1;
  }
}
