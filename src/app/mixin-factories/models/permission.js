import Ember from 'ember';
import DS from 'ember-data';

const ObjectPromiseProxy =
  Ember.ObjectProxy.extend(Ember.PromiseProxyMixin);

const {
  String: { camelize },
  RSVP: { Promise },
  computed,
  assert
} = Ember;

/**
 * A factory for creating permission models.
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
 * ## System model relations
 * 
 * The mixin will add a DS attribte for storing ID of related system model
 * (eg. system-user or system-group).
 * 
 * A computed property for fetching the attribute will be also created.
 * The system model is _not_ implemented as a ``belongsTo`` relation,
 * because fetching system model must be performed with ``findQuery``.
 * 
 * Finally, the model will have for example properties:
 * - ``systemUserId: DS.attr<string>``
 * - ``systemUser: ObjectProxy<SystemUser>``
 * 
 * ## Subject relation
 * 
 * A permission model has a relation to the subject that permissions is about.
 * For example it can be a Space or a Group.
 * 
 * The created mixin will have a ``belongsTo`` relation to the subject with
 * name of type (eg. ``space: belongsTo(space)``) and an alias to this property
 * named ``owner``.
 * 
 * @module mixin-factories/permission
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/**
 * @param {array} flagNames
 * @param {string} subjectType eg. space, group
 * @param {string} systemEntityName eg. systemUser, systemGroup
 */
function create(flagNames, subjectType, systemEntityName) {
  assert(
    'all parameters are required',
    Array.from(arguments).every(a => a)
  );

  let systemEntityNameId = systemEntityName + 'Id';

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
  let additionalAttributes = {};

  flagNames.forEach(flag => {
    additionalAttributes[camelize(`perm${flag}`)] =
      DS.attr('boolean', { defaultValue: false });
  });

  additionalAttributes[systemEntityNameId] = DS.attr('string');

  additionalAttributes[systemEntityName] = computed(
    systemEntityNameId,
    `${subjectType}.id`,
    
    function() {
      let store = this.get('store');
      
      let mainPromise = new Promise((resolve, reject) => {
        let systemEntityId = this.get(systemEntityNameId);
        let getSubject = this.get(subjectType);
        getSubject.then(subject => {
          if (!subject) {
            resolve(null);
          } else {
            let subjectId = subject.get('id');
            let query = {
              id: systemEntityId,
              context: {}
            };
            query.context[`od_${subjectType}`] = subjectId;

            let getSystemUser = store.queryRecord(systemEntityName, query);
            getSystemUser.then(resolve, reject);
          }
        });
        getSubject.catch(reject);
      });
      
      return ObjectPromiseProxy.create({
        promise: mainPromise
      });
    }
  );

  additionalAttributes[subjectType] =
    DS.belongsTo(subjectType, { async: true, inverse: null });

  additionalAttributes['owner'] = computed.alias(systemEntityName);

  mixin.reopen(additionalAttributes);

  return mixin;
}

export default create;
