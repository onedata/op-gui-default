import { moduleForComponent, test } from 'ember-qunit';
import hbs from 'htmlbars-inline-precompile';

moduleForComponent('space-settings-drop', 'Integration | Component | space settings drop', {
  integration: true
});

test('it renders', function(assert) {
  
  // Set any properties with this.set('myProperty', 'value');
  // Handle any actions with this.on('myAction', function(val) { ... });" + EOL + EOL +

  this.render(hbs`{{space-settings-drop}}`);

  assert.equal(this.$().text().trim(), '');

  // Template block usage:" + EOL +
  this.render(hbs`
    {{#space-settings-drop}}
      template block text
    {{/space-settings-drop}}
  `);

  assert.equal(this.$().text().trim(), 'template block text');
});
