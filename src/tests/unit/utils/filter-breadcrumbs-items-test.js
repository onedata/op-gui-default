/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it,
  beforeEach,
} from 'mocha';
import filterBreadcrumbsItems from 'op-worker-gui/utils/filter-breadcrumbs-items';
import generateBreadcrumbsItems from 'op-worker-gui/tests/helpers/generate-breadcrumbs-items';

const ELLIPSIS_NAME = '...';

describe('filterBreadcrumbsItems', function() {
  beforeEach(function() {
  });

  it('should return array with ellipsis and last item for count 1', function(done) {
    let numberOfFiles = 10;
    let { bitems } = generateBreadcrumbsItems(numberOfFiles);

    filterBreadcrumbsItems(bitems, 1).then(resultItems => {
      expect(resultItems.length, 'new breadcrumbs contains two elements').to.equal(2);
      expect(resultItems.get('firstObject'), 'first element of new breadcrumbs is ellipsis')
        .to.equal(bitems.get('lastObject'));
      expect(resultItems.get('firstObject'), 'first element of new breadcrumbs is last element of old breadcrumbs')
        .to.equal(bitems.get('lastObject'));
      done();
    });
  });

  it('should return array with all items (without ellipsis) if count is equal to original array length', function(done) {
    let numberOfFiles = 10;
    let { bitems } = generateBreadcrumbsItems(numberOfFiles);

    filterBreadcrumbsItems(bitems, numberOfFiles).then(resultItems => {
      expect(resultItems.length).to.equal(numberOfFiles);
      expect(resultItems.some(item => item.get('name') === ELLIPSIS_NAME)).to.be.false;
      done();
    });

  });

  it('should return array with all items (without ellipsis) if count is greater than original array length', function(done) {
    let numberOfFiles = 10;
    let { bitems } = generateBreadcrumbsItems(numberOfFiles);

    filterBreadcrumbsItems(bitems, numberOfFiles).then(resultItems => {
      expect(resultItems.length).to.equal(numberOfFiles);
      expect(resultItems.some(item => item.get('name') === ELLIPSIS_NAME)).to.be.false;
      done();
    });
  });

  it('should return array with only root element for array with only root', function(done) {
    let numberOfFiles = 1;
    let { bitems } = generateBreadcrumbsItems(numberOfFiles);

    filterBreadcrumbsItems(bitems, 10).then(resultItems => {
      expect(resultItems.length).to.equal(1);
      expect(resultItems.get('firstObject')).to.equal(bitems.get('firstObject'));
      done();
    });
  });

  it('should return array with 2 items if there are 2 items in original array', function(done) {
    let numberOfFiles = 2;
    let { bitems } = generateBreadcrumbsItems(numberOfFiles);

    filterBreadcrumbsItems(bitems, 10).then(resultItems => {
      expect(resultItems.length).to.equal(numberOfFiles);
      for (let i = 0; i < numberOfFiles; i += 1) {
        let resultItem = resultItems.objectAt(i);
        let originalItem = bitems.objectAt(i);
        expect(resultItem, `result item with name ${resultItem.get('name')} is same object as original item with name ${resultItem.get('name')}`)
          .to.equal(originalItem);
      }
      done();
    });
  });

  it('should return array with 3 items if there are 3 items in original array', function(done) {
    let numberOfFiles = 3;
    let { bitems } = generateBreadcrumbsItems(numberOfFiles);

    filterBreadcrumbsItems(bitems, 10).then(resultItems => {
      expect(resultItems.length).to.equal(numberOfFiles);
      for (let i=0; i<numberOfFiles; i+=1) {
        let resultItem = resultItems.objectAt(i);
        let originalItem = bitems.objectAt(i);
        expect(resultItem, `result item with name ${resultItem.get('name')} is same object as original item with name ${resultItem.get('name')}`)
          .to.equal(originalItem);
      }
      done();
    });
  });

  it('should return array with 4 items if there are 4 items in original array', function(done) {
    let numberOfFiles = 4;
    let { bitems } = generateBreadcrumbsItems(numberOfFiles);

    filterBreadcrumbsItems(bitems, 10).then(resultItems => {
      expect(resultItems.length).to.equal(numberOfFiles);
      for (let i=0; i<numberOfFiles; i+=1) {
        let resultItem = resultItems.objectAt(i);
        let originalItem = bitems.objectAt(i);
        expect(resultItem, `result item with name ${resultItem.get('name')} is same object as original item with name ${resultItem.get('name')}`)
          .to.equal(originalItem);
      }
      done();
    });
  });
});
