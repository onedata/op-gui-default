import DS from 'ember-data';

const {
  Model,
  hasMany,
} = DS;

export default Model.extend({
  list: hasMany('transfer', { async: true, inverse: null }),
});

// --- FIXME: MOCK - to remove in future ---

import mockHasMany from 'op-worker-gui/utils/mock-has-many';
import { mock1 as t1, mock2 as t2 } from 'op-worker-gui/models/transfer';

export const mockCurrent = {
  list: mockHasMany([t1, t2]),
};

export const mockCompleted = {
  list: mockHasMany([]),
};
