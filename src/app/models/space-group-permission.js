import Ember from 'ember';
import DS from 'ember-data';
import PermissionsModelSpaceMixin from 'op-worker-gui/mixins/permissions-model-space';

/**
 * A set of single Space permissions for a single Group
 * @module models/space-group-permission
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(PermissionsModelSpaceMixin, {
  systemGroup: DS.belongsTo('systemGroup', {async: true}),

  /** Common alias for owner - in this case group */
  owner: Ember.computed.alias('systemGroup'),
});
