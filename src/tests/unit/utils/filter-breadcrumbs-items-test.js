/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it,
  beforeEach,
} from 'mocha';
import filterBreadcrumbsItems from 'op-worker-gui/utils/filter-breadcrumbs-items';
import FileBreadcrumbsItem from 'op-worker-gui/utils/file-breadcrumbs-item';
import Ember from 'ember';

const NUMBER_OF_FILES = 10;

describe('filterBreadcrumbsItems', function() {
  beforeEach(function() {
    this.fileNames = [...Array(NUMBER_OF_FILES).keys()].map(i => `file-${i}`);
    this.files = this.fileNames.map(name => Ember.Object.create({
      name: name
    }));
    for (let i=0; i<this.files.length; i+=1) {
      this.files[i].set('parent', this.files[i-1]);
    }
    this.bitems = Ember.A(this.files.map(file => FileBreadcrumbsItem.create({
      file: file
    })));
  });

  it('should return array with ellipsis and last item for count 1', function() {
    let resultItems = filterBreadcrumbsItems(this.bitems, 1);

    expect(resultItems.length, 'new breadcrumbs contains two elements').to.equal(2);
    expect(resultItems.get('firstObject'), 'first element of new breadcrumbs is ellipsis')
      .to.equal(this.bitems.get('lastObject'));
    expect(resultItems.get('firstObject'), 'first element of new breadcrumbs is last element of old breadcrumbs')
      .to.equal(this.bitems.get('lastObject'));
  });

  // FIXME enable

  // it('should return array with all items if count is equal to original array length', function() {
  //   let resultItems = filterBreadcrumbsItems(this.bitems, NUMBER_OF_FILES);

  //   expect(resultItems.length).to.equal(NUMBER_OF_FILES);
  // });

  // it('should return array with all items if count is greater than original array length', function() {
  //   let resultItems = filterBreadcrumbsItems(this.bitems, NUMBER_OF_FILES+1);

  //   expect(resultItems.length).to.equal(NUMBER_OF_FILES);
  // });
});
