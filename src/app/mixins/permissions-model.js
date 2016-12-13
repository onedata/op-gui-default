import Ember from 'ember';

/**
 * A base DS.Model for creating permission models.
 * @module mixins/permissions-model
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/
export default Ember.Mixin.create({
  /** Sets all permission fields to original state using mod* flags */
  reset: function() {
    this.FLAG_NAMES.forEach(function(flagName) {
      var modName = 'mod' + flagName;
      var permName = 'perm' + flagName;
      if (this.get(modName)) {
        this.set(modName, !this.get(modName));
        this.set(permName, !this.get(permName));
      }
    }, this);
  },

  // TODO: move to prototype
  // Sets all mod* flags to false
  setUnmodified: function() {
    this.FLAG_NAMES.forEach(function(flagName) {
      var modName = 'mod' + flagName;
      this.set(modName, false);
    }, this);
  },
});
