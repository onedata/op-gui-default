/**
 * Redirect to spaces management in Onezone
 *
 * @module routes/onezone/providers
 * @author Jakub Liput
 * @copyright (C) 2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import RedirectBase from './-redirect-base';

export default RedirectBase.extend({
  resourceType: 'spaces',
});
