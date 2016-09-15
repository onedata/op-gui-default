import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['metadata-xml-editor'],
  classNameBindings: ['isError:parse-error'],

  isError: Ember.computed('error', function() {
    return !!this.get('error');
  }),

  error: Ember.computed('data', function() {
    let parseError = null;
    try {
      $.parseXML(this.get('data'));
    } catch (error) {
      // error is not used, because it does not have proper messages
      parseError = '!';
    }
    return parseError;
  }),
});
