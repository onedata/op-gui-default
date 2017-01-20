import Ember from 'ember';

/**
 * Factory of mixin for Model that adds checking if the record is default resource.
 * 
 * @module models/group
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

const {
  computed,
  inject
} = Ember;

/**
 * Create a Mixin for Model that adds an ``isDefault`` property.
 *
 * The property is used to check if the record is the default record
 * of particular type for User stored in session.
 * 
 * Also, setting ``false`` to the property causes this record to be not default.
 *  
 * @function create
 * @param {type} propertyId name of property in User model that stores ID of default record;
 *                          eg. ``defaultSpaceId``
 */
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
