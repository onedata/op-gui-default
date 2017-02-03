import Ember from 'ember';
import FileChunksBar from 'op-worker-gui/utils/file-chunks-bar';

const {
  run,
  observer
} = Ember;

/**
 * Draw a bar with file chunks (blocks) using function from utils.
 * @module components/file-chunks-bar
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
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

  // should use new everytime?
  redrawCanvas: observer('$canvas', 'file.size', 'fileBlocks.blocks', function() {
    let {
      file,
      fileBlocks,
      $canvas
    } = this.getProperties('$canvas', 'file', 'fileBlocks', 'canvas');

    let size = file.get('size');
    let blocks = fileBlocks.get('blocks');

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
