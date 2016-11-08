import Ember from 'ember';

import cutDirsPath from 'op-worker-gui/utils/cut-dirs-path';
import FileBreadcrumbsItem from 'op-worker-gui/utils/file-breadcrumbs-item';
import filterBreadcrumbsItems from 'op-worker-gui/utils/filter-breadcrumbs-items';

const ObjectPromiseProxy = Ember.ObjectProxy.extend(Ember.PromiseProxyMixin);

/**
 * A container for ``file-breadcrumbs-item`` items.
 * It displays a breadcrumbs path and dynamically adjusts number of breadcrumbs items.
 * @module components/file-breadcrumbs
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
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
  elementsToShow: Infinity,

  didInsertElement() {
    this.checkWidth();
    this.set('__checkWidthFun', () => {
      this.set('elementsToShow', Infinity);
      this.checkWidth();
    });
    $(window).on('resize', this.get('__checkWidthFun'));
  },

  willDestroyElement() {
    $(window).off('resize', this.get('__checkWidthFun'));
  },

  /**
   * Watch changes on properites that can cause change of width of file-breadcrumbs-list.
   * As file-breadcrumbs-list can overflow its parent, thus it can have width greater than
   * file-breadcrumbs, decrement ``elementsToShow`` count to try to fit file-breadcrumbs-list
   * into its container.
   */
  checkWidth: Ember.observer('isLoading', 'filteredBreadcrumbsItems.content.[]',
    function() {
      Ember.run.scheduleOnce('afterRender', this, () => {
        console.debug(`components/file-breadcrumbs: checking file breadcrumbs list width`);
        let $fileBreadcrumbs = this.$();
        if ($fileBreadcrumbs && $fileBreadcrumbs.length > 0) {
          let $fileBreadcrumbsList = $fileBreadcrumbs.find('.file-breadcrumbs-list');
          if ($fileBreadcrumbsList.length > 0) {
            let listWidth = $fileBreadcrumbsList.width();
            let containerWidth = $fileBreadcrumbs.width();
            let elementsToShow = this.get('elementsToShow');
            let itemsCount = this.get('filteredBreadcrumbsItems.content.length');
            if (listWidth > containerWidth) {
              if (itemsCount) {
                if (elementsToShow > itemsCount) {
                  this.set('elementsToShow', itemsCount);
                } else {
                  this.decrementProperty('elementsToShow');
                }
              }
            }
          }
        }
      });
  }),

  /**
   * @type {Ember.Array<FileBreadcrumbsItem>}
   */
  breadcrumbsItems: Ember.computed('dirsPath.[]', 'isLoading', function() {
    if (!this.get('isLoading')) {
      let dirsPath = this.get('dirsPath');
      if (dirsPath) {
        let rootId = this.get('rootDir.id') || dirsPath.get('firstObject.id');
        let items = dirsPath.map(file => {
          let fbi = FileBreadcrumbsItem.create({
            file: file,
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

  /**
   * BreadcrumbsItems filtered with ``filterBreadcrumbsItems`` function.
   * It should contain max. ``elementsToShow`` + ellipsis elements. 
   * @type {ObjectPromiseProxy<Ember.Array<FileBreadcrumbsItem>>}
   */
  filteredBreadcrumbsItems: Ember.computed('breadcrumbsItems', 'elementsToShow', function() {
    let props = this.getProperties('breadcrumbsItems', 'elementsToShow');
    return ObjectPromiseProxy.create({
      promise: new Ember.RSVP.Promise(resolve => {
        filterBreadcrumbsItems(props.breadcrumbsItems, props.elementsToShow)
          .then(items => {
            // new list of breadcrumbs items has been prepared, so we can reset 
            resolve(items);
          });
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

  isLoading: Ember.computed('dirsPath.@each.name', function() {
    let dirsPath = this.get('dirsPath');
    return !dirsPath || !this.get('dirsPath').some(d => d.get('name'));
  }),

  actions: {
    changeDir(file) {
      this.sendAction('changeDir', file);
    }
  }
});
