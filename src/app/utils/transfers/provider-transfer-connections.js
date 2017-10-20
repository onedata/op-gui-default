import _ from 'lodash';

export default function transfersProviderConnections(providerTransfers) {
  return _(providerTransfers)
    .map(itran => [itran.src, itran.dest].sort())
    .uniqWith((a, b) => _.isEqual(a, b))
    .value();
}
