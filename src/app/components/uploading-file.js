import Ember from 'ember';

const { computed } = Ember;

export default Ember.Component.extend({
  tagName: 'li',
  classNameBindings: ['resumableFileClass'],

  /**
   * To inject.
   * @type {ResumableFile}
   */
  resumableFile: null,
  
  /**
   * To inject/bind.
   * Range: 0..1 (float)
   * @type {Number}
   */
  progress: null,

  percentageProgress: computed('progress', function() {
    let p = this.get('progress');
    return Math.floor(p*100);
  }),

  resumableFileClass: computed('resumableFile', function() {
    return `resumable-file-${this.get('resumableFile.uniqueIdentifier')}`;
  }),
});