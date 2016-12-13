import Ember from 'ember';

/**
 * Allows to bind globally event handler (on, off) and trigger global events.
 * @module services/events-bus
 * @author Jakub Liput
 * @author Łukasz Opioła
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Service.extend(Ember.Evented, {});
