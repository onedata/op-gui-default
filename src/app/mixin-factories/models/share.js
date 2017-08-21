import Ember from 'ember';
import DS from 'ember-data';

/**
 * Mixin for creating shares model (eg. share, share-public)
 * @module mixin-factories/models/share
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

const {
  computed,
  assert
} = Ember;

const {
  belongsTo,
  attr
} = DS;

let FILE_RELATION = {
  regular: 'file',
  public: 'file-public'
};

let HANDLE_RELATION = {
  regular: 'handle',
  public: 'handle-public'
};

let CONTAINER_DIR_RELATION = {
  regular: 'file-shared',
  public: 'file-public'
};

/**
 * Create a Mixin for creating share-* models.
 * @param {string} type one of: regular, public
 */
function create(type) {
  let fileRelation = FILE_RELATION[type];
  let handleRelation = HANDLE_RELATION[type];
  let containerDirRelation = CONTAINER_DIR_RELATION[type];

  assert(
    'fileRelation and containerDirRelation should be not null',
    fileRelation && containerDirRelation
  );

  let mixin = Ember.Mixin.create({
    name: attr('string'),

    publicUrl: attr('string'),

    /*** RELATIONS */

    file: belongsTo(fileRelation, {
      inverse: null,
      async: true
    }),

    containerDir: belongsTo(containerDirRelation, {
      inverse: null,
      async: true
    }),

    handle: belongsTo(handleRelation, {
      async: true
    }),

    /*** RELATION CHECKS */

    hasHandle: computed('handle.content', function () {
      return this.belongsTo('handle').id() != null;
    }),
  });

  if (type === 'regular') {
    mixin.reopen({
      user: belongsTo('user', {
        async: true
      }),
      dataSpace: belongsTo('space', {
        async: true
      }),
    });
  }

  return mixin;
}

export default create;
