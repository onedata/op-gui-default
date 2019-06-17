import Ember from 'ember';

const {
  A
} = Ember;

export default Ember.Component.extend({
  /**
   * To inject.
   * A property of model: ``FileProperty.basic``
   * @type {String}
   */
  dataString: null,

  /**
   * To inject.
   * If true, metadata cannot be edited.
   * @type {Boolean}
   * @default
   */
  readOnly: false,

  /**
   * If true, basic metadata in editor is valid and thus can be saved.
   * @type {Boolean}
   */
  isValid: true,

  /**
   * One of the key of edited basic metadata can be edited (using
   * ``basic-new-entry``). We must then filter it out from ``liveData``
   * because we do not want to show it as a ``basic-entry``.
   * @type {[type]}
   */
  editedKey: null,

  /**
   * Will be initialized on init and updated on
   * entry create/remove/new entry changed.
   * @type {Ember.Array}
   */
  existingKeys: null,

  init() {
    this._super(...arguments);
    this.setProperties({
      isValid: true,
    });
    // Initialize existingKeys - because we do not want use computed property
    // that creates new array every time the data changes.
    // Further existing keys add/remove will be done on create/remove entry etc.
    let data = this.get('data') || {};
    this.set('existingKeys', A(Object.keys(data)));
  },

  /**
   * Object representation of stringified object that is injected
   * (``dataString``). It represents a basic metadata JSON.
   * @type {Object}
   */
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

  /**
   * Converts the ``data``, which is a plain object, to ``Ember.Array``
   * of arrays ([key, value]).
   * We do it, because we need ``Ember.Array``'s notifications.
   * // TODO this creates new Ember Array every time the data is changed
   * // TODO this is inefficient, because creates new array everytime the edited key is changed
   */
  liveData: Ember.computed('data', 'editedKey', {
    get() {
      const data = this.get('data') || {};
      if (data) {
        const flat = Object.keys(data)
          .filter(key => key !== this.get('editedKey'))
          .map(key => [key, data[key]]);
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

  // TODO
  // dataChanged: Ember.observer('data', function() {
  //   this.sendAction('hasBeenModified');
  // }),

  actions: {
    /**
     * Change entry edited with ``basic-new-entry`` to regular ``basic-entry``
     * and clear ``basic-new-entry`` editor. Should be invoked, when
     * "Add new entry" on the bottom is clicked.
     */
    createNewEntry(key, value, resolve) {
      let existingKeys = this.get('existingKeys');

      this.set('editedKey', null);
      existingKeys.pushObject(key);

      if (resolve) {
        resolve();
      }
    },

    removeEntry(key, resolve) {
      const tmpData = this.get('data') || {};
      delete tmpData[key];
      this.get('existingKeys').removeObject(key);
      this.set('data', tmpData);
      if (resolve) {
        resolve();
      }
    },

    /**
     * Handle action, when new entry row (``basic-new-entry``) notified about
     * changes of its value.
     * @param {String} key
     * @param {String} value
     * @param {Boolean} isValid is the entry valid - currenlty it means, that
     *                          the key does not repeat
     */
    newEntryChanged(key, value, isValid) {
      const oldKey = this.get('editedKey');
      const tmpData = this.get('data') || {};
      if (isValid) {
        if (oldKey !== key) {
          // remove old edited key, because we got new key
          delete tmpData[this.get('editedKey')];
          this.set('editedKey', key);
        }
        tmpData[key] = value;
        this.set('isValid', true);        
      } else {
        this.set('isValid', false);
        delete tmpData[oldKey];
      }

      this.set('data', tmpData);
    },

    // TODO: disabled, because we do not allow to edit not-new entries
    // /**
    //  * Handle notification about change of some of entries.
    //  */
    // entryChanged(key, value) {
    //   const data = this.get('data');
    //   if (key in data) {
    //     data[key] = value;
    //     this.set('data', data);
    //   }
    // },
  }
});
