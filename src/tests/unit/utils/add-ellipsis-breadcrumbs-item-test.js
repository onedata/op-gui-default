/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it,
  beforeEach,
} from 'mocha';
import addEllipsisBreadcrumbsItem from 'op-worker-gui/utils/add-ellipsis-breadcrumbs-item';
import Ember from 'ember';
import FileBreadcrumbsItem from 'op-worker-gui/utils/file-breadcrumbs-item';

function generateBreadcrumbsItems(number_of_files) {
  let result = {};
  result.fileNames = [...Array(number_of_files).keys()].map(i => `file-${i}`);
  result.files = result.fileNames.map(name => Ember.Object.create({
    name: name
  }));
  for (let i=0; i<result.files.length; i+=1) {
    result.files[i].set('parent', result.files[i-1]);
  }
  result.bitems = Ember.A(result.files.map(file => FileBreadcrumbsItem.create({
    file: file
  })));
  return result;
}

describe('addEllipsisBreadcrumbsItem', function() {
  beforeEach(function() {

  });

  it('adds ellipsis item whose file point to parent of its right neighbour', function() {
    let number_of_files = 10;
    let { bitems } = generateBreadcrumbsItems(number_of_files);
    let itemIndex = 3;
    let item = bitems.objectAt(itemIndex);
    let parentFile = item.get('file.parent');
    let updatedItems = addEllipsisBreadcrumbsItem(bitems, item);
    let ellipsisItem = updatedItems.objectAt(itemIndex);

    expect(updatedItems.length).to.equal(number_of_files+1);
    expect(ellipsisItem.get('file')).to.equal(parentFile);
  });
});
