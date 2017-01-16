import Ember from 'ember';
import DS from 'ember-data';

import FileBaseMixin from 'op-worker-gui/mixins/models/file-base';
import FileRuntimeMixin from 'op-worker-gui/mixins/models/file-runtime';

const {
  belongsTo,
  hasMany
} = DS;

const {
  assert
} = Ember;

// values: [<name of relaion model>, <has inverse?>]
const SHARE_RELATIONS = {
  regular: ['share', false],
  shared: ['share', false],
  public: ['share-public', true]
};

const PARENT_RELATIONS = {
  regular: 'file',
  shared: 'file-shared',
  public: 'file-public'
};

const CHILDREN_RELATIONS = {
  regular: 'file',
  shared: 'file-shared',
  public: 'file-public'
};

const FILE_PROPERTY_RELATIONS = {
  regular: 'file-property',
  shared: 'file-property-shared',
  public: 'file-property-public'
};

/**
 * Common attributes and methods of file and file-public models.
 * @module mixin-factories/models/file
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default create;

/**
 * @function create
 * @param {string} type one of:
 *                      - regular (for File model)
 *                      - shared (for FileShared model)
 *                      - public (for FilePublic model)
 */
function create(type) {
  let relations = {};

  let [shareModel, shareHasInverse] = SHARE_RELATIONS[type];
  let parentModel = PARENT_RELATIONS[type];
  let childrenModel = CHILDREN_RELATIONS[type];
  let filePropertyModel = FILE_PROPERTY_RELATIONS[type];

  assert(
    'shareModel, parentModel and childernModel should be not null',
    shareModel && parentModel && childrenModel && filePropertyModel
  );

  relations['share'] = belongsTo(shareModel, {
    inverse: shareHasInverse ? 'file' : null,
    async: true
  });

  relations['parent'] = belongsTo(parentModel, {
    inverse: 'children',
    async: true
  });

  relations['children'] = hasMany(parentModel, {
    inverse: 'parent',
    async: true
  });

  relations['fileProperty'] = belongsTo(filePropertyModel, {
    inverse: 'file',
    async: true
  });

  return Ember.Mixin.create(FileBaseMixin, FileRuntimeMixin, relations);
}
