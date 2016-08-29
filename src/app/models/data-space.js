import DS from 'ember-data';

/**
 * A space for files. It has a reference to root dir with it's files.
 * @module models/data-space
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  /** Name exposed in GUI */
  name: DS.attr('string'),
  isDefault: DS.attr('boolean'),

  /** A root directory with space files. It must be a "dir" file! */
  rootDir: DS.belongsTo('file', {async: true}),

  space: DS.belongsTo('space', {async: true}),

  save() {
    const p = this._super(...arguments);
    p.then(() => {
      this.get('space').then(s => s.update());
    });
    return p;
  }

  // // TODO this does not work because does not loads rootDir...
  // validateRootDir: function() {
  //   let rootDir = this.get('rootDir');
  //   if (!rootDir) {
  //     console.error(`DataSpace ${this.get('id')} rootDir is null or undefinded!`);
  //   } else if (!rootDir.get('isDir')) {
  //     console.error(`DataSpace ${this.get('id')} rootDir has been set to non dir file!`);
  //   }
  // }.observes('rootDir')
});
