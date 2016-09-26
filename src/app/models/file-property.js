import DS from 'ember-data';

export default DS.Model.extend({
  file: DS.belongsTo('file', {async: true}),
  basic: DS.attr('object-string', { defaultValue: '{}'}),
  json: DS.attr('object-string', { defaultValue: '{}'}),
  rdf: DS.attr('string', { defaultValue: ''} ),
});
