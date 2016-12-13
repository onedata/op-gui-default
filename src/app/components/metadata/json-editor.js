import Ember from 'ember';

export default Ember.Component.extend({
  classNames: ['metadata-json-editor'],
  classNameBindings: ['isError:parse-error'],

  dataString: null,

  /**
   * To inject.
   * If true, content cannot be edited.
   * @type {Boolean}
   * @default
   */
  readOnly: false,

  error: null,

  isError: Ember.computed('error', function() {
    return !!this.get('error');
  }),

  data: Ember.computed('dataString', {
    get() {
      return JSON.parse(this.get('dataString'));
    },
    set(key, value) {
      this.set('dataString', JSON.stringify(value));
      this.notifyPropertyChange('data');
      return value;
    }
  }),

  // TODO: maybe we shouldn't serialize/deserialize in fly
  prettyData: Ember.computed('data', {
    get() {
      const json = this.get('data');
      return JSON.stringify(json, null, 2);
    },
    set(key, value) {
      try {
        const json = JSON.parse(value);
        this.set('data', json);
        this.set('error', null);
      } catch (error) {
        console.debug(`JSON editor parse error: ${error}`);
        this.set('error', error);
      }

      return value;
    }
  })
});
