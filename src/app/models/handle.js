import DS from 'ember-data';

import HandleMixin from 'op-worker-gui/mixins/models/handle';

const {
  belongsTo
} = DS;

/**
 * Represents a public handle for resource (currently a public shared dir).
 * Version for authorized view.
 * @module models/handle
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend(HandleMixin, {
  share: belongsTo('share', {
    async: true
  }),
});
