import FileBreadcrumbsItem from 'op-worker-gui/utils/file-breadcrumbs-item';
import Ember from 'ember';

/**
 * @function
 * Adds "ellipsis item" to breadcrumbs ``items`` for ``child``.
 * For example, if we got breadcrumbs: ``a > b > c > d > e``,
 * and we invoke function for child ``c``, the items will be altered to:
 * ``a > b > parent_of_c > c > d > e``.
 * 
 * This function should be used for incomplete paths, where ``b``
 * is not a parent of ``c``.
 * 
 * NOTE that it changes ``items`` contents.
 * @param {Ember.A<FileBreadcrumbsItem>} items
 * @param {FileBreadcrumbsItem} child
 * @returns {Ember.A<FileBreadcrumbsItem>} reference to altered ``items``
 */
function addEllipsisBreadcrumbsItem(items, child) {
  let ellipsisFile = child.get('file.parent');
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

export default addEllipsisBreadcrumbsItem;
