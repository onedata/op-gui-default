import Ember from 'ember';

/**
 * Renders a ``one-checkbox-button`` for permissions change purposes.
 *
 * @module components/file-permissions/acl
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  i18n: Ember.inject.service(),

  init() {
    this._super();

    const perm = `ace.perm_${this.get('name')}`;
    this.checked = Ember.computed.alias(perm);
  },

  /**
   * Computes i18n label using name.
   */
  label: function() {
    return this.get('i18n').t(
      'components.filePermissions.acl.aceItem.permissions.' +
      this.get('name')
    );
  }.property('name'),

  /**
   * To inject.
   *
   * A permissions name as in ``utils/acl-utils`` stating with ``perm_``.
   *
   * @type String
   */
  name: null,

  /**
   * To inject.
   *
   * @type AccessControlEntity
   */
  ace: null
});
