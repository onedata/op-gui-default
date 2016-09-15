import DS from 'ember-data';

export default DS.Model.extend({
  file: DS.belongsTo('file', {inverse: 'meta', async: true}),
  basic: DS.attr('object', {defaultValue: {}}),
  json: DS.attr('object', {defaultValue: {}}),
  rdf: DS.attr('string'),
});
