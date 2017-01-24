import Ember from 'ember';
import layout from '../templates/components/settings-drop-item';

/**
 * An item in ``settings-drop`` component.
 * Action should be bound externally using action helper.
 * @module components/settings-drop-item
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'li',
  classNames: ['clickable'],

  layout,

  icon: null,
  label: null,
});
