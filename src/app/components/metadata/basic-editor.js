import Ember from 'ember';

export default Ember.Component.extend({
  data: null,

  init() {
    this._super(...arguments);
    debugger;
  },

  liveData: Ember.computed('data', {
    get() {
      const data = this.get('data');
      const flat = Object.keys(data).map(key => [key, data[key]]);
      return Ember.A(flat);
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
});
