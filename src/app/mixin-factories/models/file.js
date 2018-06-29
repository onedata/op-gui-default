import Ember from 'ember';
import DS from 'ember-data';

import FileBaseMixin from 'op-worker-gui/mixins/models/file-base';
import FileRuntimeMixin from 'op-worker-gui/mixins/models/file-runtime';
import FileTransfersMixin from 'op-worker-gui/mixins/models/file-transfers';

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
 * A factory method for creating specific File models.
 * Factory is needed, because specific File models have relations to
 * specific models (different shares, parents, children and file-properites).
 * 
 * Uses ``file-base`` and ``file-runtime`` mixins.
 * 
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
    inverse: null,
    async: true
  });

  relations['children'] = hasMany(parentModel, {
    inverse: null,
    async: true
  });

  relations['fileProperty'] = belongsTo(filePropertyModel, {
    inverse: 'file',
    async: true
  });

  return Ember.Mixin.create(
    FileBaseMixin, 
    FileRuntimeMixin, 
    FileTransfersMixin,
    relations
  );
}
