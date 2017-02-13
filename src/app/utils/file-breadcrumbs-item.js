import Ember from 'ember';

/**
 * An envelope Ember Class for ``FileBreadcrumbs``.
 * @module utils/file-breadcrumbs-item
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

/**
 * @class
 * A model for ``FileBreadcrumbs`` component used
 * mainly internally in file breadcrumbs.
 */
const FileBreadcrumbsItem = Ember.Object.extend({
  /**
   * @required
   * @type {File}
   */
  file: null,

  /**
   * Overriden name of file.
   * Do not set it manually - use ``set('name')``.
   */
  __name: undefined,

  /**
   * A name of item displayed in breadcrumbs.
   * By default it uses ``file.name`` computed property.
   * If set, the name is overriden but original ``file.name`` is untouched.
   * 
   * NOTE that it can be shortened with CSS ellipsis later before rendering.
   */
  name: Ember.computed('__name', 'file.name', {
    get() {
      return this.get('__name') || this.get('file.name');
    },
    set(key, value) {
      this.set('__name', value);
      return this.get('__name');
    }
  }),

  __isRoot: undefined,

  // TODO: automatic isRoot detection seems not to work with template
  isRoot: Ember.computed('__isRoot', 'file.hasParent', {
    get() {
      let __isRoot = this.get('__isRoot');
      if (this.get('__isRoot') === undefined) {
        return this.get('file.hasParent') === false;
      } else {
        return __isRoot;
      }
    },
    set(key, value) {
      this.set('__isRoot', value);
      return this.get('__isRoot');
    }
  }),
});

export default FileBreadcrumbsItem;
