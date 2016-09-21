import DS from 'ember-data';

export default DS.Model.extend({
  file: DS.belongsTo('file', {async: true}),
  basic: DS.attr('object', {defaultValue: () => {}}),
  json: DS.attr('object', {defaultValue: () => {}}),
  rdf: DS.attr('string'),

  // HACK: force update of object attributes as it is not managed by Ember
  // FIXME: this should really detect changes in these properties
  changedAttributes() {
    return Object.assign(this._super(), {
      basic: [undefined, this.get('basic')],
      json: [undefined, this.get('json')]
    });
  },
});
