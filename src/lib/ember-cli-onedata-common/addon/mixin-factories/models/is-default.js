import Ember from 'ember';

// FIXME jsdoc

const {
  computed,
  inject
} = Ember;

function create(propertyId) {
  return Ember.Mixin.create({
    session: inject.service(),

    isDefault: computed('id', '__idOfDefault', {
      get() {
        return this.__checkIsDefault();
      },
      set(key, value) {
        if (value === true) {
          this.set('__idOfDefault', this);
        } else {
          if (this.__checkIsDefault()) {
            this.set('__idOfDefault', null);
          }
        }
      }
    }),

    __idOfDefault: computed.alias('session.user.' + propertyId),

    __checkIsDefault() {
      return this.get('__idOfDefault') === this.get('id');
    }
  });
}

export default create;
