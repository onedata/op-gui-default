import Ember from 'ember';
import DS from 'ember-data';

const {
  belongsTo,
  attr
} = DS;

export default Ember.Mixin.create({
  metadataString: attr('string'),
  publicHandle: attr('string'),

  handleService: belongsTo('handleService', {
    inverse: null,
    async: true
  }),
  
});
