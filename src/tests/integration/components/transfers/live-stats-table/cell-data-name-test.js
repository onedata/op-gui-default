import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';

describe('Integration | Component | transfers/live stats table/cell data name', function() {
  setupComponentTest('transfers/live-stats-table/cell-data-name', {
    integration: true
  });

  it('renders file icon and file name for file', function() {
    this.set('record', {
      path: '/one/two/three.txt',
      dataSourceType: 'file'
    });
    this.render(hbs`{{transfers/live-stats-table/cell-data-name record=record}}`);
    const $component = this.$('.cell-data-name');
    expect($component.find('.transfer-file-name')).to.have.text('three.txt');
    expect($component.find('.transfer-file-icon')).to.have.class('oneicon-file');
  });
});
