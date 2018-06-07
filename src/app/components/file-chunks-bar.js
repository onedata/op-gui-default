import Ember from 'ember';
import FileChunksBar from 'op-worker-gui/utils/file-chunks-bar';

const {
  run,
  observer,
  get,
  computed,
} = Ember;

/**
 * Draw a bar with file chunks (blocks) using function from utils.
 * @module components/file-chunks-bar
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  classNames: ['file-chunks-bar'],
  
  file: null,
  fileBlocks: null,

  didInsertElement() {
    this._super(...arguments);
    run.scheduleOnce('afterRender', this, function() {
      this.set('$canvas', this.$('canvas'));
      this.redrawCanvas();
    });
  },

  isRendered: false,
  isRenderFailed: false,
  isLoading: false,

  /**
   * @type {Ember.ComputedProperty<number>}
   */
  chunksPercent: computed('file.size', 'fileBlocks.blocks', function () {
    const size = this.get('file.size');
    const blocks = this.get('fileBlocks.blocks');
    if (size && blocks) {
      let sum = 0;
      for (let i = 0; i < blocks.length; i += 2) {
        sum += blocks[i + 1] - blocks[i];
      }
      return Math.floor(sum / size * 1000) / 10;
    } else {
      return undefined;
    }
  }),

  // should use new everytime?
  redrawCanvas: observer('$canvas', 'file.size', 'fileBlocks.blocks', function() {
    let {
      file,
      fileBlocks,
      $canvas
    } = this.getProperties('$canvas', 'file', 'fileBlocks', 'canvas');

    let size = get(file, 'size');
    let blocks = get(fileBlocks, 'blocks');

    if (size && blocks) {
      try {
        this.setProperties({
          isRenderFailed: false,
          isLoading: true
        });
        new FileChunksBar($canvas, {
          file_size: size,
          chunks: blocks
        });
        this.setProperties({
          isRendered: true,
          isLoading: false
        });
      } catch (error) {
        this.set('isRenderFailed', false);
      }
    }
  })
});
