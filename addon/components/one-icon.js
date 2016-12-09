import {
  Component,
  computed
} from 'ember';
import layout from 'ember-cli-onedata-common/templates/components/one-icon';

// TODO: make a common component with op-worker; not there are component and helpers
/**
 * Inserts a icon from oneicons font.
 * Typical usage: ``{{one-icon icon='home'}}``
 * @module components/one-icon
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Component.extend({
  layout,

  tagName: 'span',
  classNames: ['one-icon'],
  classNameBindings: ['iconClass', 'colorClass', 'additionalClasses'],

  iconClass: computed('icon', function() {
    return `oneicon-${this.get('icon')}`;
  }),

  colorClass: computed('color', function() {
    let color = this.get('color');
    return color ? `color-${this.get('color')}` : '';
  }),

  // TODO: this should be removed in favor of "class" property
  additionalClasses: computed('addClass', function() {
    return this.get('addClass');
  }),

  // defaults
  icon: 'checkbox-x',
  color: '',
  addClass: '',
});
