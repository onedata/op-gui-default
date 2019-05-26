import Ember from 'ember';
import DS from 'ember-data';

const {
  attr
} = DS;

/**
 * Base data store attributes of File models.
 * See ``mixin-factories/models/file`` for creating specific models.
 * @module mixins/models/file-base
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Mixin.create({
  name: attr('string'),

  /**
   * Specifies is this object a regular file ("file") or directory ("dir")
   * To check if it is a dir please use "isDir" property.
   */
  type: attr('string'),

  modificationTime: attr('number'),
  size: attr('number'),

  /**
   * ID of Oneprovider that stores this file.
   * @type {Ember.computed<String>}
   */
  provider: attr('string'),

  /**
   * How many children this directory (it it is a directory-type) has.
   * If ``totalChildrenCount`` is more than actual ``children.length``, it means
   * that more children can be fetch from server.
   *
   * See also: ``oneproviderServer.fetchMoreDirChildren``.
   *
   * See also: ``allChildrenLoaded`` computed property.
   */
  totalChildrenCount: attr('number'),
  
  /**
   * If true, user can browser contents of directory
   */
  canViewDir: attr('boolean'),
});
