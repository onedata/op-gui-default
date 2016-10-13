import Ember from 'ember';
import RouteRejectHandler from 'op-worker-gui/mixins/route-reject-handler';

/**
 * Load a single dir (File model) and show a file browser for it (passed as route name).
 * @module routes/data/data-space/dir
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Route.extend(RouteRejectHandler, {
  fileSystemTree: Ember.inject.service(),
  notify: Ember.inject.service(),
  commonLoader: Ember.inject.service(),

  fallbackRoute: 'onedata.data.data-space.index',

  model(params) {
    // TODO: check if loaded dir belongs to loaded space (data/data-space model)?
    const p = this.handleReject(this.store.findRecord('file', params.dir_id));
    p.finally(() => this.set('commonLoader.isLoading', false));
    return p;
  },

  afterModel(file/*, transition*/) {
    this.set('invalid', false);

    if (file.get('isDeleted')) {
      console.error('Loaded file is deleted');
      // TODO: translate
      this.get('notify').error(`Cannot start file browser, because selected directory is marked as deleted`);
      this.set('invalid', true);
    }

    if (!file.get('isDir')) {
      console.error('Loaded file is not a directory - it cannot be viewed in browser');
      // TODO: translate
      this.get('notify').error(`Cannot start file browser, because selected directory is not valid`);
      this.set('invalid', true);
    }

    // @todo this sometimes runs too early and getSpaceIdForFile does not work
    //let loadedDirSpaceId = this.get('fileSystemTree').getSpaceIdForFile(file);
    //if (loadedDirSpaceId !== this.modelFor('onedata.data.data-space').get('id')) {
    //  console.error('Space of loaded dir (file) is not a space loaded in data-space route');
    //  transition.abort();
    //}
  },

  /**
    This is a directory browser. It can show only one directory contents at a time.
    Render it in "data" template, because it's a master view of a data browser.
  */
  renderTemplate() {
    if (this.get('invalid')) {
      this.render('onedata.data.dataSpace.dir.error', {
        into: 'onedata.data',
        outlet: 'dir'
      });
    } else {
      this.render('onedata.data.dataSpace.dir.dirToolbar', {
        into: 'onedata',
        outlet: 'toolbar'
      });
      this.render({
        into: 'onedata.data',
        outlet: 'dir'
      });
    }
  },

  /**
   * actionOnReject - Adds a failed dir to set of failed dirs in fileSystemTree
   *
   * @param {String} data An ID of dir that failed to load
   */
  actionOnReject(err, data) {
    this.get('fileSystemTree.failedDirs').add(data);

    this._super();
  },

  actions: {
    loading() {
      this.get('commonLoader').setProperties({
        isLoading: true,
        message: this.get('i18n').t('data.dataSpace.dir.loaderMessage'),
        area: 'content-with-secondary-top'
      });
    },

    /**
     * Open either create-file-share or share-info modal for specified
     * File record, which should be a directory.
     *
     * @param {File} file
     */
    openFileShareModal(file, resolveAction) {
      const p = file.get('share').then(
        (share) => {
          if (share) {
            // we already got a Share, show info
            this.controller.set('isShowingShareInfo', true);
          } else {
            // we do not have a Share - open create modal
            this.controller.set('isCreatingShare', true);
          }
          if (resolveAction) {
            resolveAction();
          }
        },
        () => {
          this.controller.set('isCreatingShare', true);
          if (resolveAction) {
            resolveAction();
          }
        }
      );

      p.finally(() => {
        this.controller.setProperties({
          fileShareFile: file,
          share: null,
        });
      });
    },

    openShareInfoModal(share) {
      if (share) {
        this.controller.setProperties({
          fileShareFile: null,
          share: share,
          isShowingShareInfo: true,
        });
      }

    }
  }

});
