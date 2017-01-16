import DS from 'ember-data';

const {
  attr,
  hasMany
} = DS;

// FIXME jsdoc
export default DS.Model.extend({
  name: attr('string'),
  defaultSpaceId: attr('string'),

  /*** Relations ***/

  groups: hasMany('group', {
    async: true,
    inverse: null
  }),

  spaces: hasMany('spaces', {
    async: true,
    inverse: null
  }),

  shares: hasMany('share', {
    async: true,
    inverse: null
  }),

  handleServices: hasMany('handle-service', {
    async: true,
    inverse: null
  }),
});
