import { test } from 'qunit';
import moduleForAcceptance from 'op-worker-gui/tests/helpers/module-for-acceptance';

moduleForAcceptance('Acceptance | example');

test('visiting /example', function(assert) {
  visit('/example');

  andThen(function() {
    assert.equal(currentURL(), '/example');
  });
});
