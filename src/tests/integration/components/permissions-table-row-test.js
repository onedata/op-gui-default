import { moduleForComponent, test } from 'ember-qunit';
import hbs from 'htmlbars-inline-precompile';

moduleForComponent('permissions-table-row', 'Integration | Component | permissions table row', {
  integration: true
});

test('it renders', function(assert) {
  
  // Set any properties with this.set('myProperty', 'value');
  // Handle any actions with this.on('myAction', function(val) { ... });" + EOL + EOL +

  this.render(hbs`{{permissions-table-row}}`);

  assert.equal(this.$().text().trim(), '');

  // Template block usage:" + EOL +
  this.render(hbs`
    {{#permissions-table-row}}
      template block text
    {{/permissions-table-row}}
  `);

  assert.equal(this.$().text().trim(), 'template block text');
});
