/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import FileBreadcrumbsItem from 'op-worker-gui/utils/file-breadcrumbs-item';
import Ember from 'ember';

describe('FileBreadcrumbsItem', function() {
  it('uses file name as its name by default', function() {
    let file = Ember.Object.create({
      name: 'hello'
    });
    let fbi = FileBreadcrumbsItem.create({
      file: file
    });

    expect(fbi.get('name')).to.equal(file.get('name'));
  });
  
  it('allows to set custom name without altering file name', function() {
    let file = Ember.Object.create({
      name: 'hello'
    });
    let fbi = FileBreadcrumbsItem.create({
      file: file,
      name: 'world'
    });

    expect(fbi.get('name'), 'FBItem name is customized').to.equal('world');
    expect(file.get('name'), 'file name stays the same').to.equal('hello');
  });
});
