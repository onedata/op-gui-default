/**
 * Resolves a space that should be loaded by default
 * when entering index of spaces or data-spaces.
 * 
 * @module utils/get-default-space
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';

const {
  get,
  RSVP: { reject },
} = Ember;
 
export default function getDefaultSpace(spaces, currentProviderId) {
  let selectedSpace;
  let defaultSpace = spaces.find((s) => s.get('isDefault'));
  if (defaultSpace) {
    selectedSpace = defaultSpace;
  } else {
    console.debug('util:get-default-space: No default data-space found - choose data-space instead');
    const firstSpace = spaces.sortBy('name').objectAt(0);
    if (firstSpace) {
      selectedSpace = firstSpace;
    } else {
      return reject('util:get-default-space: No data-spaces exist');
    }
  }
  const orderedSpaces = _.without(spaces.toArray(), selectedSpace).sortBy('name');
  orderedSpaces.unshift(selectedSpace);
  return resolveSupportedSpace(orderedSpaces, 0, currentProviderId);
}

function resolveSupportedSpace(spaces, i, currentProviderId) {
  const selectedSpace = spaces[i];
  if (selectedSpace) {
    return get(selectedSpace, 'providerList')
    .then(providerList => {
      const supportingProviderIds = get(providerList, 'list');
      if (supportingProviderIds.indexOf(currentProviderId) !== -1) {
        return selectedSpace;
      } else {
        return resolveSupportedSpace(spaces, i + 1, currentProviderId);
      }
    }); 
  } else {
    return null;
  }
}
