import Ember from 'ember';

export default Ember.Component.extend({
  /**
   * "basic" property of metadata record.
   * @type {Object}
   */
  data: null,

  // FIXME
  // /**
  //  * Original injected data, saved on init for comparison on data changes.
  //  * @type {Object}
  //  */
  // originalData: null,
  dataIsDirty: false,

  // FIXME
  // init() {
  //   this._super(...arguments);
  //   this.set('originalData', this.get('data'));
  // },
  //
  // dataChanged: Ember.observer('data', 'originalData', function() {
  //   // FIXME: two objects comparison effectiveness
  //   const dataIsDirty = (JSON.stringify(this.get('data')) !== JSON.stringify(this.get('originalData')));
  //   this.set('dataIsDirty', dataIsDirty);
  // }),
  //
  // dataIsDirtyChanged: Ember.observer('dataIsDirty', function() {
  //   this.sendAction('dataIsDirtyChanged', this.get('dataIsDirty'));
  // }),

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
