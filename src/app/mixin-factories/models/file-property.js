import Ember from 'ember';
import DS from 'ember-data';

const {
  belongsTo,
  attr
} = DS;

const FILE_MODELS = {
  public: 'file-public',
  shared: 'file-shared'
};

/**
 * @param {string} type one of: regular, shared, public
 */
function create(type) {
  let fileModel = (FILE_MODELS[type] || 'file');

  let mixin = Ember.Mixin.create({
    file: belongsTo(fileModel, {async: true}),
    basic: attr('object-string', { defaultValue: '{}'}),
    json: attr('object-string', { defaultValue: '{}'}),
    rdf: attr('string', { defaultValue: ''} ),
  });

  return mixin;
}

export default create;
