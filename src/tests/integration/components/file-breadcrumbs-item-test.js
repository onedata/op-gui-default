/* jshint expr:true */
import Ember from 'ember';
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import {
  beforeEach,
  afterEach
} from 'mocha';
import startApp from 'op-worker-gui/tests/helpers/start-app';
import hbs from 'htmlbars-inline-precompile';

describeComponent(
  'file-breadcrumbs-item',
  'Integration: FileBreadcrumbsItemComponent',
  {
    integration: true
  },
  function() {
    beforeEach(function () {
      this.application = startApp();
      this.store = this.application.__container__.lookup('service:store');
    });

    afterEach(function() {
      Ember.run(this.application, 'destroy');
    });

    it('renders', function() {
      // Set any properties with this.set('myProperty', 'value');
      // Handle any actions with this.on('myAction', function(val) { ... });
      // Template block usage:
      // this.render(hbs`
      //   {{#file-breadcrumbs-item}}
      //     template content
      //   {{/file-breadcrumbs-item}}
      // `);

      this.render(hbs`{{file-breadcrumbs-item}}`);
      expect(this.$()).to.have.length(1);
    });

    it('has a link that clicked sends changeDir action', function(done) {
      const file = Ember.Object.create({
        id: 'hello-file',
      });

      this.set('file', file);
      this.on('externalChangeDir', (file) => {
        expect(file).to.be.ok;
        expect(file.get('id')).to.be.equal(file.get('id'));
        done();
      });

      this.render(hbs`{{file-breadcrumbs-item file=file changeDir="externalChangeDir"}}`);

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

      this.set('file', f1);

      this.render(hbs`{{file-breadcrumbs-item file=file}}`);

      expect(this.$().find('.file-breadcrumbs-next-icon')).to.exist;
    });

    it('does not render an arrow icon before text if file has not a parent', function() {
      const f0 = this.store.createRecord('file', {
        name: 'file-0',
      });

      this.set('file', f0);

      this.render(hbs`{{file-breadcrumbs-item file=file}}`);

      expect(this.$().find('.file-breadcrumbs-next-icon')).not.to.exist;
    });
  }
);
