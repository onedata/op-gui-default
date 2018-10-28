import Ember from 'ember';

const {
  Component,
  computed,
} = Ember;

export default Component.extend({  
  indexName: undefined,
  spaceName: undefined,
  spatial: undefined,
  providerNames: undefined,
  indexOptions: undefined,
  
  providers: computed('providerNames.[]', function providers() {
    const providerNames = this.get('providerNames');
    if (providerNames) {
      return providerNames.join(', ');
    }
  }),
  
  displayedProperties: Object.freeze([
    'indexName',
    'spaceName',
    'providers',
    'spatial',
  ]),
});
