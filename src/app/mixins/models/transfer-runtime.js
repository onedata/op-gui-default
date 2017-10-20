import Ember from 'ember';

import _ from 'lodash';

const {
  Mixin,
  computed,
  get,
} = Ember;

export default Mixin.create({
  // TODO: there will be problems with recomputing "transferred" property
  // - it is too complex for watching changes
  sources: computed('stats', function () {
    const stats = this.get('stats');
    return _.union(
      ..._.map(
        ['hour', 'day', 'month'],
        period => Object.keys(get(stats, period))
      )
    );
  }),
});
