import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | modals/file chunks/migrate popover', function () {
  setupComponentTest('modals/file-chunks/migrate-popover', {
    integration: true
  });

  it('renders providers list except source provider', function () {
    const p1 = {
      id: 'p1',
      name: 'Provider 1',
    };
    const p2 = {
      id: 'p2',
      name: 'Provider 2',
    };
    const p3 = {
      id: 'p3',
      name: 'Provider 3',
    };
    const providers = [p1, p2, p3];
    const sourceProvider = p2;
    this.set('providers', providers);
    this.set('sourceProvider', sourceProvider);

    this.render(hbs `{{modals/file-chunks/migrate-popover
      providers=providers sourceProvider=sourceProvider
      bindSelector=null
    }}
    `);

    const $migratePopover = this.$('.migrate-popover');
    expect($migratePopover).to.exist;
    expect($migratePopover).to.contain('Provider 1');
    expect($migratePopover).to.not.contain('Provider 2');
    expect($migratePopover).to.contain('Provider 3');
  });
});
