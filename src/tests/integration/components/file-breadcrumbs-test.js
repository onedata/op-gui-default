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
  'file-breadcrumbs',
  'Integration: FileBreadcrumbsComponent',
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
      //   {{#file-breadcrumbs}}
      //     template content
      //   {{/file-breadcrumbs}}
      // `);

      this.render(hbs`{{file-breadcrumbs}}`);
      expect(this.$()).to.have.length(1);
    });

    it('prints name of the injected file', function () {
      const file = this.store.createRecord('file', {
        name: 'hello'
      });

      this.set('file', file);

      this.render(hbs`{{file-breadcrumbs file=file}}`);

      expect(this.$()).to.contain(file.get('name'));
    });

    it('prints space name of the injected file', function () {

    });
  }
);
