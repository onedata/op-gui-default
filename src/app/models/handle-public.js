import DS from 'ember-data';

import HandleMixin from 'op-worker-gui/mixins/models/handle';

/**
 * Represents a public handle for resource (currently a public shared dir).
 * Version for public view.
 * @module models/handle-public
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(HandleMixin);
