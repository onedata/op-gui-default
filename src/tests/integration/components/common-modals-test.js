import { moduleForComponent, test } from 'ember-qunit';
import hbs from 'htmlbars-inline-precompile';

import startApp from '../../helpers/start-app';
import destroyApp from '../../helpers/destroy-app';

let application;

moduleForComponent('common-modals', 'Integration | Component | common modals', {
  integration: true,

  beforeEach() {
    console.log('before');
    // we must start app to invoke initializers for bs-modal
    application = startApp();
  },

  afterEach() {
    console.log('after');
    destroyApp(application);
  }
});

test('it renders', function(assert) {

  // Set any properties with this.set('myProperty', 'value');
  // Handle any actions with this.on('myAction', function(val) { ... });" + EOL + EOL +

  this.render(hbs`{{common-modals}}`);

  assert.expect(0);
  // assert.equal(this.$().text().trim(), '');


});
