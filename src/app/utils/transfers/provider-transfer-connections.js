/**
 * Reduce transfers data to create just pairs of current connections between providers
 * caused by transfers
 *
 * @module utils/transfers/provider-transfer-connections
 * @author Jakub Liput, Michal Borzecki
 * @copyright (C) 2017-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import _ from 'lodash';

/**
 * Generates collection of connection between two providers
 * Order in connection is random; each pair can occur once.
 * @param {object} mapping object with fields:
 * sourceProviderId -> [destinationProviderId]
 * @return {Array<Array>} each array element has 2 elements, eg.
 *    `[['a', 'b'], ['c', 'a'], ['b', 'c']]`
 */
export default function transfersProviderConnections(mapping) {
  return _(Object.keys(mapping))
    .flatMap(sourceId => mapping[sourceId].map(destId => [sourceId, destId].sort()))
    .uniqWith((a, b) => _.isEqual(a, b))
    .value();
}
