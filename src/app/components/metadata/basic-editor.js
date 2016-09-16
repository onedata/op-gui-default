import Ember from 'ember';

export default Ember.Component.extend({
  data: null,

  init() {
    this._super(...arguments);
  },

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
      this.get('data')[key] = value;
      this.notifyPropertyChange('data');
      if (resolve) {
        resolve();
      }
    },
    removeEntry(key, resolve) {
      delete this.get('data')[key];
      this.notifyPropertyChange('data');
      if (resolve) {
        resolve();
      }
    }
  }
});
