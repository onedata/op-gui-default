import Ember from 'ember';

import cutDirsPath from 'op-worker-gui/utils/cut-dirs-path';
import FileBreadcrumbsItem from 'op-worker-gui/utils/file-breadcrumbs-item';
import filterBreadcrumbsItems from 'op-worker-gui/utils/filter-breadcrumbs-items';

const ObjectPromiseProxy = Ember.ObjectProxy.extend(Ember.PromiseProxyMixin);

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
  elementsToShow: 6,

  // FIXME: a function/observer/whatever to set elementsToShow
  // based on available space in breadcrumbs

  /**
   * @type {Ember.A<FileBreadcrumbsItem>}
   */
  breadcrumbsItems: Ember.computed('dirsPath.[]', 'isLoading', 'elementsToShow', function() {
    if (!this.get('isLoading')) {
      let dirsPath = this.get('dirsPath');
      if (dirsPath) {
        let rootId = this.get('rootDir.id') || dirsPath.get('firstObject.id');
        let items = dirsPath.map(file => {
          let fbi = FileBreadcrumbsItem.create({
            file: file,
            // isRoot: rootId ? file.get('id') === rootId : undefined
            // FIXME: forcing use of predefined isRoot flag
            isRoot: file.get('id') === rootId
          });
          return fbi;
        });
        return items;
      } else {
        return null;
      }
    } else {
      return null;
    }
  }),

  filteredBreadcrumbsItems: Ember.computed('breadcrumbsItems', 'elementsToShow', function() {
    let props = this.getProperties('breadcrumbsItems', 'elementsToShow');
    return ObjectPromiseProxy.create({
      promise: new Ember.RSVP.Promise(resolve => {
        filterBreadcrumbsItems(props.breadcrumbsItems, props.elementsToShow)
          .then(items => resolve(items));
      })
    });
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
