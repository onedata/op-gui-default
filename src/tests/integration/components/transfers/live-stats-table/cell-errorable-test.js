import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | transfers/live stats table/cell errorable', function() {
  setupComponentTest('transfers/live-stats-table/cell-errorable', {
    integration: true
  });

  it('renders value if currentStatError record flag false', function() {
    const record = {
      something: 'value',
    };
    const column = {
      propertyName: 'something',
    };
    this.setProperties({
      record,
      column,
    });
    this.render(hbs`
      {{transfers/live-stats-table/cell-errorable record=record column=column}}
    `);

    expect(this.$().text(), this.$().html()).to.match(/value/);
  });
  
  it('renders error component when there is currentStatError record flag true', function() {
    const record = {
      something: 'value',
      currentStatError: true,
    };
    const column = {
      propertyName: 'something',
    };
    this.setProperties({
      record,
      column,
    });
    this.render(hbs`
      {{transfers/live-stats-table/cell-errorable record=record}}
    `);

    expect(this.$('.error-inline'), this.$().html()).to.exist;
  });
});
