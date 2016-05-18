import Ember from 'ember';

/**
 * A component for debug puroposes - it should not be included in application normally.
 * It creates an element with debug information - currently about service properties.
 * @module components/service-debugger
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  secondaryMenu: Ember.inject.service(),

  classNames: ['service-debugger'],
});
