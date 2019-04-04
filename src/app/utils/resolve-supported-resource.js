/**
 * Find first record, which is supported by current Oneprovider
 * 
 * @module utils/resolve-supported-resource
 * @author Jakub Liput
 * @copyright (C) 2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 * 
 * @param {Array<Model>} collection array of records, to check which is supported
 *  by current provider
 * @param {Number} i index of next record to check
 * @param {*} currentProviderId this Oneprovider ID
 * @param {*} resolveSupportingProviders `(record) => Promise<Array<string>>` should resolve with
 *  array of Oneprovider IDs that supports the `record`
 */
export default function resolveSupportedResource(
  collection,
  i,
  currentProviderId,
  resolveSupportingProviders
) {
  const selectedResource = collection[i];
  if (selectedResource) {
    return resolveSupportingProviders(selectedResource)
      .then(supportingProviderIds => {
        if (supportingProviderIds.indexOf(currentProviderId) !== -1) {
          return selectedResource;
        } else {
          return resolveSupportedResource(
            collection,
            i + 1,
            currentProviderId,
            resolveSupportingProviders
          );
        }
      });
  } else {
    return null;
  }
}
