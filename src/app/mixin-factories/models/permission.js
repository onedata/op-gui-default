import Ember from 'ember';
import DS from 'ember-data';

/**
 * A factiory for creating permission models.
 * 
 * Created model has persisted attributes based on ``FLAG_NAMES`` in this file, eg.
 * ```
 * permViewGroup: DS.attr('boolean', {defaultValue: false}),
 * ```
 * and runtime properties that indicate flag modification, eg.:
 * ```
 * modViewGroup: false,
 * ```
 * 
 * @module mixin-factories/permission
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

const {
  String: { camelize },
  computed
} = Ember;

const DEFAULT_FALSE = { defaultValue: false };

function create(flagNames) {
  let modFlags = flagNames.map(flag => camelize(`mod${flag}`));

  let mixin = Ember.Mixin.create({
    init() {
      this._super(...arguments);

      // Modification flags (not persisted) - if true, the corresponding perm* 
      // attribte was modified in view but not saved.
      // Note that these flags are not mainained on perm* flags change. Maybe TODO      
      modFlags.forEach(modFlag => {
        this[modFlag] = false;
      });
    },

   /**
    * Checks if Permission is modified using mod* flags
    * @returns {boolean} true if at least one mod* flag is true
    */
    isModified: computed(...modFlags, function() {
      let flagValues = Object.values(this.getProperties(...modFlags));
      return flagValues.indexOf(true) !== -1;
    }),

    /**
     * Sets all permission fields to original state using mod* flags
     */
    reset() {
      flagNames.forEach(flagName => {
        var modName = 'mod' + flagName;
        var permName = 'perm' + flagName;
        if (this.get(modName)) {
          this.set(modName, !this.get(modName));
          this.set(permName, !this.get(permName));
        }
      });
    },

    /**
     * Sets all mod* flags to false
     */
    setUnmodified() {
      flagNames.forEach(flagName => {
        var modName = 'mod' + flagName;
        this.set(modName, false);
      });
    },
  });

  // Permission flags - grant permission when true.
  // Note that corresponding mod* flag should be modified on these flag changes.
  
  // Create model persisted attribute for single permission flag
  // Eg. permViewSpace: DS.attr('boolean', {defaultValue: false}),
  flagNames.forEach(flag => {
    mixin[camelize(`perm${flag}`)] =
      DS.attr('boolean', DEFAULT_FALSE);
  });
}

export default create;
