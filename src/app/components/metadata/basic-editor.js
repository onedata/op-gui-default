import Ember from 'ember';

export default Ember.Component.extend({
  dataString: null,

  /**
   * Original injected data, saved on init for comparison on data changes.
   * @type {Object}
   */
  originalDataString: null,

  init() {
    this._super(...arguments);
    this.set('originalDataString', this.get('dataString'));
  },

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

  dataIsDirty: Ember.computed('dataString', 'originalDataString', function() {
    return this.get('dataString') !== this.get('originalDataString');
  }),

  /**
   * Converts the ``data``, which is a plain object, to ``Ember.Array``
   * of arrays ([key, value]).
   * We do it, because we need ``Ember.Array``'s notifications.
   */
  liveData: Ember.computed('data', {
    get() {
      const data = this.get('data');
      if (data) {
        const flat = Object.keys(data).map(key => [key, data[key]]);
        return Ember.A(flat);
      } else {
        return Ember.A();
      }
    },
    set(key, value) {
      const arr = value;
      const obj = arr.reduce((prev, curr) => {
        prev[curr[0]] = curr[1];
        return prev;
      }, {});
      this.set('data', obj);
      return obj;
    }
  }),

  actions: {
    createNewEntry(key, value, resolve) {
      // FIXME: validate - do not allow duplicate keys
      const tmpData = this.get('data');
      tmpData[key] = value;
      this.set('data', tmpData);
      // this.notifyPropertyChange('data');
      if (resolve) {
        resolve();
      }
    },
    removeEntry(key, resolve) {
      const tmpData = this.get('data');
      delete tmpData[key];
      this.set('data', tmpData);
      // this.notifyPropertyChange('data');
      if (resolve) {
        resolve();
      }
    }
  }
});
