import Ember from 'ember';

const {
  Component,
  computed,
} = Ember;

export default Component.extend({  
  viewName: undefined,
  spaceName: undefined,
  spatial: undefined,
  providerNames: undefined,
  viewOptions: undefined,
  
  providers: computed('providerNames.[]', function providers() {
    const providerNames = this.get('providerNames');
    if (providerNames) {
      return providerNames.join(', ');
    }
  }),
  
  displayedProperties: Object.freeze([
    'viewName',
    'spaceName',
    'providers',
    'spatial',
  ]),
});
