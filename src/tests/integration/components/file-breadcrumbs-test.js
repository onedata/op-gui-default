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

describe('Integration: FileBreadcrumbsComponent', function() {
  setupComponentTest('file-breadcrumbs', {
    integration: true
  });

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

  // TODO: below 2 tests have problems with async that occurs randomly on
  // in-docker tests; no solution found for now
  
  // it('displays name of the injected file', function() {
  //   const file = this.store.createRecord('file', {
  //     name: 'hello1'
  //   });
  //   this.set('file', file);
  //   this.render(hbs`{{file-breadcrumbs file=file}}`);
  //   let fileName = file.get('name');
  //   file.updateDirsPath();
  //   return wait().then(() => {
  //     expect(this.$()).to.contain(fileName);
  //   });
  // });

  // it('displays parent name of the injected file', function() {
  //   const file1 = this.store.createRecord('file', {
  //     name: 'hello01'
  //   });
  //   const file2 = this.store.createRecord('file', {
  //     parent: file1,
  //     name: 'hello02'
  //   });
  //   this.set('file2', file2);
  //   this.render(hbs`{{file-breadcrumbs file=file2}}`);
  //   let fileName = file1.get('name');
  //   file2.updateDirsPath();
  //   return wait().then(() => {
  //     expect(this.$()).to.contain(fileName);
  //   });
  // });

  it('does not displays parent path dirs above specified rootDir', function(done) {
    const file1 = this.store.createRecord('file', {
      name: 'hello1'
    });
    const file2 = this.store.createRecord('file', {
      parent: file1,
      name: 'hello2'
    });
    const file3 = this.store.createRecord('file', {
      parent: file2,
      name: 'hello3'
    });
    const file4 = this.store.createRecord('file', {
      parent: file3,
      name: 'hello4'
    });

    this.set('file3', file3);
    this.set('file4', file4);

    this.render(hbs`{{file-breadcrumbs file=file4 rootDir=file3}}`);

    let doneCalled = false;

    file2.addObserver('dirsPath', this, function() {
      Ember.run.scheduleOnce('afterRender', this, function() {
        if (file2.get('dirsPath')) {
          expect(this.$()).to.not.contain(file1.get('name'));
          expect(this.$()).to.not.contain(file2.get('name'));
          if (!doneCalled) {
            doneCalled = true;
            done();
          }
        }
      });
    });

    file4.updateDirsPath();
  });
});
