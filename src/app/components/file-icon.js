import Ember from 'ember';

export default Ember.Component.extend({
  /**
   * A File that will be presented
   * @type File
   */
  file: null,

  /**
   * File icons are both used on lists and trees.
   * On the tree, we have an open/closed dir indicator, which is not
   * displayed on lists
   *
   * Allowed values: "list", "tree"
   * @type String
   */
  containerType: 'list',

  iconName: Ember.computed(
    'file.isDir', 'file.hasShare', 'file.share', 'file.isExpanded',
    'containerType',
    function() {
      const isDir = this.get('file.isDir');
      const hasShare = this.get('file.hasShare');
      const isExpanded = this.get('file.isExpanded');
      const containerType = this.get('containerType');
      if (isDir) {
        if (containerType === 'tree' && isExpanded) {
          // TODO: icon for shared folder expanded?
          return 'folder-open';
        } else {
          if (hasShare) {
            // FIXME: new icon for shared folder
            return 'folder-share';
          } else {
            return 'folder';
          }
        }
      } else {
        return 'file';
      }
    }),

  iconClass: Ember.computed('file.isDir', 'file.isBroken', function() {
    const isDir = this.get('file.isDir');
    const isBroken = this.get('file.isBroken');
    if (isBroken || isDir) {
      return 'color-directory';
    } else {
      return 'color-file';
    }
  }),
});
