import FileBreadcrumbsItem from 'op-worker-gui/utils/file-breadcrumbs-item';
import Ember from 'ember';

// TODO: use ellipsis char: &#8230;
const ELLIPSIS_NAME = '...';

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
 * @returns {RSVP.Promise<Ember.A<FileBreadcrumbsItem>>} resolves with reference to altered ``items``
 */
function addEllipsisBreadcrumbsItem(items, child) {
  return new Ember.RSVP.Promise(resolve => {
    Ember.assert(child, 'child item cannot be null or undefined');
    let hasEllipsisFile = child.get('file.hasParent');
    if (hasEllipsisFile) {
      child.get('file.parent').then(ellipsisFile => {
        let childIndex = items.indexOf(child);
        let originalParent = items.objectAt(childIndex-1);
        if (originalParent && originalParent.get('file.id') === ellipsisFile.get('id')) {
          console.debug(`utils/add-ellipsis-breadcrumbs-item: An ellipsis item will be not added for ${child.get('name')}, because its parent is already in array`);
          resolve(items);
        } else {
          Ember.assert(
            childIndex > -1,
            'when adding ellipsis item, the child of ellipsis item should be present in items array'
          );
          let ellipsisItem = FileBreadcrumbsItem.create({
            file: ellipsisFile,
            name: ELLIPSIS_NAME,
            isRoot: (childIndex === 0)
          });
          items.splice(childIndex, 0, ellipsisItem);
          resolve(items);
        }
      });
    } else {
      console.debug(`utils/add-ellipsis-breadcrumbs-item: An ellipsis item will be not added for ${child.get('name')}, because it has no parent`);
      resolve(items);
    }
  });
}

export default addEllipsisBreadcrumbsItem;
