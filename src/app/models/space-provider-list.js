import DS from 'ember-data';
import Ember from 'ember';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';

const {
  Model,
  attr,
  belongsTo,
} = DS;

const {
  computed,
  RSVP: { Promise },
} = Ember;

export default Model.extend({
  list: attr('array'),
  space: belongsTo('space', { async: true }),
    
  /**
   * @type {Ember.ComputedProperty<PromiseObject<SystemProvider>>}
   */
  queryList: computed('id', 'list.[]', function () {
    const {
      id,
      list,
    } = this.getProperties('id', 'list');
    if (list) {
      return PromiseObject.create({
        promise: Promise.all(list.map(providerId =>
          this.store.queryRecord('system-provider', {
            id: providerId,
            context: {
              od_space: id,
            },
          })
        )),
      }); 
    }
  }),
});
