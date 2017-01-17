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
    async: true
  }),

  spaces: hasMany('spaces', {
    async: true
  }),

  shares: hasMany('share', {
    async: true
  }),

  handleServices: hasMany('handle-service', {
    async: true
  }),
});
