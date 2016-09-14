import DS from 'ember-data';

/**
 * A space for files. It has a reference to root dir with it's files.
 * @module models/data-space
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  /** A root directory with space files. It must be a dir-type File! */
  rootDir: DS.belongsTo('file', {async: true}),

  /** Name exposed in GUI */
  name: DS.attr('string'),
  isDefault: DS.attr('boolean'),

  space: DS.belongsTo('space', {async: true}),

  save() {
    const p = this._super(...arguments);
    p.then(() => {
      this.get('space').then(s => s.update());
    });
    return p;
  }
});
