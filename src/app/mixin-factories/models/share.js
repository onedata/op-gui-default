import Ember from 'ember';
import DS from 'ember-data';

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

let CONTAINER_DIR_RELATION = {
  regular: 'file-shared',
  public: 'file-public'
};

// FIXME jsdoc
function create(type) {
  let fileRelation = FILE_RELATION[type];
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

    handle: belongsTo('handle', {
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

// FIXME jsdoc
export default create;
