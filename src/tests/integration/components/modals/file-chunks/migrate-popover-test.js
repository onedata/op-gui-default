import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

import Ember from 'ember';

const {
  get,
} = Ember;

const P1 = {
  id: 'p1',
  name: 'Provider 1',
};
const P2 = {
  id: 'p2',
  name: 'Provider 2',
};
const P3 = {
  id: 'p3',
  name: 'Provider 3',
};

describe('Integration | Component | modals/file chunks/migrate popover', function () {
  setupComponentTest('modals/file-chunks/migrate-popover', {
    integration: true
  });

  it('renders providers list except source provider', function () {
    const providers = [P1, P2, P3];
    const sourceProvider = P2;
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
  
  it('disables disabledProviders on list and adds a "busy" text', function () {
    const p3Id = get(P3, 'id');
    const providers = [P1, P2, P3];
    const sourceProvider = P2;
    const disabledProviderIds = [p3Id];
    this.setProperties({
      providers,
      sourceProvider,
      disabledProviderIds,
    });

    this.render(hbs `{{modals/file-chunks/migrate-popover
      providers=providers
      sourceProvider=sourceProvider
      disabledProviderIds=disabledProviderIds
      bindSelector=null
    }}
    `);

    const $migratePopover = this.$('.migrate-popover');
    const $migrateToP3 = $migratePopover.find(`.migrate-item-provider-${p3Id}`);
    expect(
      $migrateToP3,
      'migrate to p3 item'
    ).to.exist;
    expect($migrateToP3, 'migrate to p3 item').to.have.class('disabled');
    expect($migrateToP3.text()).to.match(/busy/);
  });
});
