/**
 * Model with on-the-fly transfers list for space.
 * @module models/space-on-the-fly-transfer-list
 * @author Michal Borzecki
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import DS from 'ember-data';

const {
  Model,
  hasMany,
} = DS;

export default Model.extend({
  list: hasMany('on-the-fly-transfer', { async: true, inverse: null }),
});
