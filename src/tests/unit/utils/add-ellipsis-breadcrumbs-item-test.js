/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import addEllipsisBreadcrumbsItem from 'op-worker-gui/utils/add-ellipsis-breadcrumbs-item';
import generateBreadcrumbsItems from 'op-worker-gui/tests/helpers/generate-breadcrumbs-items';

describe('addEllipsisBreadcrumbsItem', function() {
  it('adds ellipsis item whose file point to parent of its right neighbour', function(done) {
    let numberOfFiles = 10;
    let { bitems } = generateBreadcrumbsItems(numberOfFiles);
    let itemIndex = 5;
    let item = bitems.objectAt(itemIndex);
    // remove original parent of item
    bitems.removeAt(itemIndex-1);
    item.get('file.parent').then(parentFile => {
      addEllipsisBreadcrumbsItem(bitems, item).then(updatedItems => {
        let ellipsisItem = updatedItems.objectAt(itemIndex-1);
        expect(updatedItems.length).to.equal(numberOfFiles);
        expect(ellipsisItem.get('file.id')).to.equal(parentFile.get('id'));
        done();
      });
    });
  });

  it('does not add ellipsis for item that has no parent', function(done) {
    let { bitems } = generateBreadcrumbsItems(1);
    let item = bitems.get('firstObject');

    addEllipsisBreadcrumbsItem(bitems, item).then(updatedItems => {
      expect(updatedItems.length).to.equal(1);
      expect(updatedItems.get('firstObject')).to.equal(item);
      done();
    });
  });

  it('does not add ellipsis for item if item parent is already predecessor of item in original array', function(done) {
    let numberOfFiles = 10;
    let { bitems } = generateBreadcrumbsItems(numberOfFiles);
    let item = bitems.objectAt(5);

    addEllipsisBreadcrumbsItem(bitems, item).then(updatedItems => {
      expect(updatedItems.length).to.equal(numberOfFiles);
      expect(updatedItems.objectAt(5)).to.equal(item);
      done();
    });
  });
});
