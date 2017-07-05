/**
 * A function to poll for condition for given time.
 * If the condition is met, invoke "resolve" function.
 * If the condition is not met in given timeout, invoke "reject" function.
 * @module utils/wait-for
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
function waitFor(conditionFun, { resolve, reject, interval=200, timeout=2000 }) {
  let checkId = window.setInterval(() => {
    if (conditionFun()) {
      window.clearTimeout(checkId);
      if (resolve) {
        resolve();
      }
    }
  }, interval);
  window.setTimeout(() => {
    window.clearInterval(checkId);
    if (reject) {
      reject();
    }
  }, timeout);
}

export default waitFor;