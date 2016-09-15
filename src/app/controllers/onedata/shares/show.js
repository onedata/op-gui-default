import Ember from 'ember';

export default Ember.Controller.extend({
  secondaryMenu: Ember.inject.service(),

  /**
   * Can be both File or Ember.ObjectProxy of File.
   * Please do not use this - use ``directory`` property instead.
   * @private
   */
  directory: null,

  /**
   * Watch change of Share, because we want to change current directory in
   * files browser.
   */
  modelChanged: Ember.observer('model', function() {
    this.set('directory', this.get('model.file'));
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

    createMetadata() {
      const file = this.get('model.file');
      file.then(
        f => {
          const metadata = this.get('store').createRecord('file-property', {
            basic: {},
            json: {},
            rdf: "",
            file: f,
          });
          f.set('fileProperty', metadata);
          metadata.save().then(
            () => f.save()
          );
        }
      );
    }
  }
});
