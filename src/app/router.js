/**
 * Defines Ember routes - see code of Router.map callback function for details.

 * @module router
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import config from './config/environment';

const Router = Ember.Router.extend({
  location: config.locationType
});

Router.map(function() {
  // spaces/ - all spaces configuration reached from primary sidebar
  this.route('spaces', {resetNamespace: true}, function() {
    // spaces/:space_id - entry for configuration of the single space
    this.route('show', {path: ':space_id'}, function() {
      // spaces/:space_id/users - configure users permissions for space
      this.route('users');
      // spaces/:space_id/groups - configure groups permissions for space
      this.route('groups');
    });
  });

  // data/ - list of Spaces - user can select the Space
  this.route('data', {resetNamespace: true}, function() {
    // data/:space_id - show dir tree of a root dir of selected Space
    this.route('data-space', {path: '/:data_space_id'}, function() {
      // data/:space_id/:dir_id - a directory, which content is displayed
      //   currently in browser (and is expanded in data-files-tree)
      this.route('dir', {path: '/:dir_id'}, function() {
        // data/:space_id/:dir_id/:file_id - file selected in a browser
        this.route('file', {path: '/:file_id'});
      });

    });
  });

  // groups/ - all groups configuration reached from primary sidebar
  this.route('groups', {resetNamespace: true}, function() {
    // groups/:group_id - entry for configuration of the single space
    this.route('show', {path: ':group_id'}, function() {
      // groups/:group_id/members - configure users/groups (members) permissions for space
      this.route('members');
    });
  });

  // shared/ - browse Shares reached from primary sidebar
  this.route('shared', {resetNamespace: true}, function() {
      this.route('show', {path: ':share_id'});
  });

  // TODO: activate after routes implementation
  // this.route('recent', {resetNamespace: true});
  // this.route('links', {resetNamespace: true});
  // this.route('trash', {resetNamespace: true});

  // handle routes not handled above and langauge-prefixed paths (e.g. /en/spaces)
  this.route('wildcard', { path: "*path"});
});

export default Router;
