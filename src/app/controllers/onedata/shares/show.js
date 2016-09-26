import Ember from 'ember';

export default Ember.Controller.extend({
  secondaryMenu: Ember.inject.service(),

  /**
   * Can be both File or Ember.ObjectProxy of File.
   */
  directory: null,

  /**
   * If true, open the modal, which is for publishing this share.
   * @type {Boolean}
   */
  publishShareModalOpened: false,

  /**
   * Watch change of Share, because we want to change current directory in
   * files browser.
   */
  modelChanged: Ember.observer('model', function() {
    this.set('directory', this.get('model.containerDir'));
    if (this.get('model')) {
      this.changeMenuActiveItem();
    }
  }),

  changeMenuActiveItem() {
    this.set('secondaryMenu.activeItem', this.get('model'));

    // TODO: use property binding
    Ember.run.scheduleOnce('afterRender', this, function() {
      $('nav.secondary-sidebar').addClass('visible');
    });
  },

  actions: {
    openDirInBrowser(file) {
      this.set('directory', file);
    },

    openDataDir(dirFile, reject) {
      this.get('model.dataSpace').then(
        (dataSpace) => {
          this.transitionToRoute('onedata.data.data-space.dir', dataSpace, dirFile);
        },
        (error) => reject && reject(error)
      );
    },

    publishShare() {
      this.set('publishShareModalOpened', true);
    }
  }
});
