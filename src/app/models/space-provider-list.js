import DS from 'ember-data';

const {
  Model,
  hasMany,
} = DS;

export default Model.extend({
  list: hasMany('system-provider'),
});

// --- FIXME: MOCK - to remove in future ---

import mockHasMany from 'op-worker-gui/utils/mock-has-many';
import { 
  mock1 as p1, 
  mock2 as p2, 
  mock3 as p3,
  mock4 as p4,
} from 'op-worker-gui/models/system-provider';

export const mockProviders = {
  list: mockHasMany([p1, p2, p3, p4]),
};
