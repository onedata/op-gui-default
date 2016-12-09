import Ember from 'ember';
import layout from 'ember-cli-onedata-common/templates/components/one-radio-group';

/**
 * A container for ``one-radio-button``s.
 *
 * @module components/one-radio-group
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  layout,
  classNames: ['one-option-group'],
});
