import Ember from 'ember';

import cutDirsPath from 'op-worker-gui/utils/cut-dirs-path';

const FileBreadcrumbsItem = Ember.Object.extend({
  file: null,
  name: null,
  isRoot: false,
});

function addEllipsisItem(items, child) {
  let ellipsisFile = child.get('parent');
  if (ellipsisFile) {
    let childIndex = items.indexOf(child);
    Ember.assert(
      childIndex > -1,
      'when adding ellipsis item, the child of ellipsis item should be present in items array'
    );
    let ellipsisItem = FileBreadcrumbsItem.create({
      file: ellipsisFile,
      // TODO: use ellipsis char: &#8230;
      name: '...',
      isRoot: (childIndex === 0)
    });
    items.splice(childIndex, 0, ellipsisItem);
  }
  return items;
}

/**
 * @function
 * Filter elements of given breadcrumbs items array
 * to get array with shortened breadcrumbs.
 * @returns {Ember.A<FileBreadcrumbsItem>} shortened breadcrumbs items array
 */
function filterBreadcrumbsItems(items, count) {
  let resultArray = Ember.A();
  let itemsCount = items.get('length');
  if (count > 0) {
    // add last element (current dir)
    resultArray.push(items.objectAt(itemsCount-1));
  } else {
    return resultArray;
  }
  if (count > 1) {
    // add root item at start of items
    resultArray.splice(0, 0, items.objectAt(0));
  } else {
    // only one element, but add ellipsis item if can
    return addEllipsisItem(resultArray, items.objectAt(0));
  }
  if (count > 2 ) {
    // add parent of current dir before current dir
    resultArray.splice(1, 0, (items.objectAt(itemsCount-2)));
  } else {
    return addEllipsisItem(resultArray, items.objectAt(itemsCount-2));
  }
  if (count > 3) {
    // add first child of root
    resultArray.splice(1, 0, items.objectAt(1));
  } else {
    return addEllipsisItem(resultArray, items.objectAt(1));
  }
  if (count > 4 && itemsCount >= 4) {
    let lastItemToAddIndex = itemsCount - 2;
    let firstItemToAddIndex = lastItemToAddIndex - (count - 4);
    // first item should not be lower than first child of root
    // because we already added it earlier
    firstItemToAddIndex = Math.max(2, firstItemToAddIndex);
    resultArray = Ember.A(
      resultArray.slice(0, 2).concat(
        items.slice(firstItemToAddIndex, lastItemToAddIndex).concat(
          resultArray.slice(resultArray(3, 5))
        )
      )
    );
  }
  // FIXME: probably there will be bug with 4 elements (additional ellipsis)
  // FIXME: add last ellipsis element addEllipsisElement()
  return resultArray;
}

export default Ember.Component.extend({
  classNames: ['file-breadcrumbs'],

  /**
   * File, which path to will be presented.
   * Can be a directory of course.
   * @type {File}
   */
  file: null,

  /**
   * Optional: if provided, breadcrumbs will use ``rootDir`` as a root of path
   * (if provided)
   * @type {File}
   */
  rootDir: null,

  /**
   * How many breadcrumbs items should be rendered.
   * A special element: (...) is always additionally rendered,
   * so there will be N+1 elements visible.
   * @type {Number}
   */
  elementsToShow: 4,

  // FIXME: a function/observer/whatever to set elementsToShow
  // based on available space in breadcrumbs

  /**
   * @type {Ember.A<FileBreadcrumbsItem>}
   */
  breadcrumbsItems: Ember.computed('dirsPath.[]', 'isLoading', 'elementsToShow', function() {
    if (!this.get('isLoading')) {
      let dirsPath = this.get('dirsPath');
      if (dirsPath) {
        let rootId = this.get('rootDir.id');
        let items = dirsPath.map(file => {
          return FileBreadcrumbsItem.create({
            file: file,
            name: file.get('name'),
            isRoot: file.get('id') === rootId
          });
        });
        return filterBreadcrumbsItems(items, this.get('elementsToShow'));
      } else {
        return null;
      }
    } else {
      return null;
    }
  }),

  dirsPath: Ember.computed('file.dirsPath.[]', 'rootDir', function() {
    const dpath = this.get('file.dirsPath');
    if (dpath) {
      const rdir = this.get('rootDir');
      return rdir ? cutDirsPath(dpath, rdir) : dpath;
    } else {
      return null;
    }
  }),

  isLoading: Ember.computed('dirsPath', function() {
    let dirsPath = this.get('dirsPath');
    return !dirsPath || !this.get('dirsPath').some(d => d.get('name'));
  }),

  actions: {
    changeDir(file) {
      this.sendAction('changeDir', file);
    }
  }
});
