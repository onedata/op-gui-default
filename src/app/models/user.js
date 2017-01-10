import DS from 'ember-data';

const {
  attr,
  belongsTo,
  hasMany
} = DS;

const ASYNC_ONEWAY = { async: true, inverse: null };

// FIXME jsdoc
export default DS.Model.extend({
  name: attr('string'),
  alias: attr('string'),

  /*** Relations ***/

  defaultSpace: belongsTo('space', ASYNC_ONEWAY),

  groups: hasMany('group', ASYNC_ONEWAY),
  spaces: hasMany('spaces', ASYNC_ONEWAY),
  shares: hasMany('share', ASYNC_ONEWAY),
  handleServices: hasMany('handle-service', ASYNC_ONEWAY),
});
