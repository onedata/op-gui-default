/**
 * Redirect to client tokens management in Onezone
 *
 * @module routes/onezone/tokens
 * @author Jakub Liput
 * @copyright (C) 2017-2019 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import RedirectBase from './-redirect-base';

export default RedirectBase.extend({
  resourceType: 'tokens',
});
