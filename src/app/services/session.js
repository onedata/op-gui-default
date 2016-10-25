import SessionOnedata from './session-onedata';
import SessionFake from './session-fake';

import ENV from 'op-worker-gui/config/environment';

let Session;
if (['test', 'localstorage'].indexOf(ENV.environment) !== -1) {
  Session = SessionFake;
} else {
  Session = SessionOnedata;
}

export default Session;
