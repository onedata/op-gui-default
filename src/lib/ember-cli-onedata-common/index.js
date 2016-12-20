/*jshint node:true*/
const MODULE_NAME = 'ember-cli-onedata-common';

module.exports = {
  name: MODULE_NAME,

  //// An example of included function
  // included: function(app, parentAddon) {
  //   var inc = this._super.included.apply(this, arguments);
  //   var target = (parentAddon || app);
  //   // target.options.sassOptions.includePaths.push(`lib/${MODULE_NAME}/app/styles/oneicons`);
  //   return inc;
  // },

  isDevelopingAddon: function() {
    return true;
  },

  /**
   * Make public files from this addon to be merged into application's public.
   */
  treeForPublic: function() {
    var tree = this._super.treeForPublic.apply(this, arguments);
    if (tree) {
      tree.destDir = '/';
    }
    return tree;
  }
};
