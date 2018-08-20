import Ember from 'ember';
import FileChunksBar from 'op-worker-gui/utils/file-chunks-bar';

const {
  run,
  observer,
  get,
  computed,
} = Ember;

const barWidth = 320;
const barHeight = 20;

/**
 * Draw a bar with file chunks (blocks) using function from utils.
 * @module components/file-chunks-bar
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  classNames: ['file-chunks-bar'],
  
  /**
   * @virtual
   * Needs:
   * - id
   * - sizeHumanReadable
   * - size
   */
  file: null,
  
  /**
   * @virtual
   * Format: `{ start px of bar [0-319]: green opacity [0-100] }`
   * @type {object}
   */
  chunksBarData: undefined,

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
  chunksShare: computed('chunksBarData', function () {
    const chunksBarData = this.get('chunksBarData');
    if (chunksBarData) {
      let sum = 0;
      
      let fragmentStarts = Object.keys(chunksBarData).map(x => parseInt(x));
      fragmentStarts.sort((x, y) => x - y);
      if (fragmentStarts.length !== 0 &&
        fragmentStarts[fragmentStarts.length - 1] !== barWidth) {
          fragmentStarts.push(barWidth);
      }
      fragmentStarts = fragmentStarts.map(i => String(i));
      
      for (let i = 0; i < fragmentStarts.length - 1; i += 1) {
        const fragmentWidth = fragmentStarts[i + 1] - fragmentStarts[i];
        sum += chunksBarData[fragmentStarts[i]] * fragmentWidth;
      }
      return sum / barWidth;
    } else {
      return undefined;
    }
  }),
  /**
   * @type {Ember.ComputedProperty<string>}
   */
  chunksPercent: computed('chunksShare', function chunksPercent() {
    const chunksShare = this.get('chunksShare');
    if (chunksShare > 0 && chunksShare < 0.1) {
      return '< 0.1%';
    } else {
      return `${Math.round(chunksShare * 10) / 10}%`;
    }
  }),

  // should use new everytime?
  redrawCanvas: observer('$canvas', 'file.size', 'chunksBarData', function() {
    let {
      file,
      chunksBarData,
      $canvas
    } = this.getProperties('$canvas', 'file', 'chunksBarData', 'canvas');

    let size = get(file, 'size');

    if (size && chunksBarData) {
      try {
        this.setProperties({
          isRenderFailed: false,
          isLoading: true
        });
        new FileChunksBar($canvas, {
          fileSize: size,
          chunksBarData,
          width: barWidth,
          height: barHeight,
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
