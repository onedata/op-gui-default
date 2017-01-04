/* jshint expr:true */
import Ember from 'ember';
import { expect } from 'chai';
import { setupComponentTest } from 'ember-mocha';
import {
  beforeEach,
  afterEach,
  it,
  describe
} from 'mocha';
import startApp from 'op-worker-gui/tests/helpers/start-app';
import hbs from 'htmlbars-inline-precompile';

import FileBreadcrumbsItem from 'op-worker-gui/utils/file-breadcrumbs-item';

describe('Integration: FileBreadcrumbsItemComponent', function() {
  setupComponentTest('file-breadcrumbs-item', {
    integration: true
  });

  beforeEach(function () {
    this.application = startApp();
    this.store = this.application.__container__.lookup('service:store');
  });

  afterEach(function() {
    Ember.run(this.application, 'destroy');
  });

  it('has a link that clicked sends changeDir action', function(done) {
    const file = Ember.Object.create({
      id: 'hello-file',
    });

    const fbi = FileBreadcrumbsItem.create({
      file: file
    });

    this.set('item', fbi);

    this.on('externalChangeDir', (file) => {
      expect(file).to.be.ok;
      expect(file.get('id')).to.be.equal(file.get('id'));
      done();
    });

    this.render(hbs`{{file-breadcrumbs-item item=item changeDir="externalChangeDir"}}`);

    this.$().find('.file-breadcrumb-item-link').click();
  });

  it('renders an arrow icon before text if file has a parent', function() {
    const f0 = this.store.createRecord('file', {
      name: 'file-0',
    });
    const f1 = this.store.createRecord('file', {
      name: 'file-1',
      parent: f0,
    });

    const fbi = FileBreadcrumbsItem.create({
      file: f1
    });

    this.set('item', fbi);

    this.render(hbs`{{file-breadcrumbs-item item=item}}`);

    expect(this.$().find('.file-breadcrumbs-next-icon')).to.exist;
  });

  it('does not render an arrow icon before text if file has not a parent', function() {
    const f0 = this.store.createRecord('file', {
      name: 'file-0',
    });

    const fbi = FileBreadcrumbsItem.create({
      file: f0
    });

    this.set('item', fbi);

    this.render(hbs`{{file-breadcrumbs-item item=item}}`);

    expect(this.$().find('.file-breadcrumbs-next-icon')).not.to.exist;
  });
});
