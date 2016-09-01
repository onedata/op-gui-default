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

  // /onedata - root route for authenticated users
  this.route('onedata', {resetNamespace: true}, function() {

    // onedata/spaces/ - all spaces configuration reached from primary sidebar
    this.route('spaces', function() {
      // onedata/spaces/:space_id - entry for configuration of the single space
      this.route('show', {path: ':space_id'}, function() {
        // onedata/spaces/:space_id/users - configure users permissions for space
        this.route('users');
        // onedata/spaces/:space_id/groups - configure groups permissions for space
        this.route('groups');
      });
    });

    // onedata/data/ - list of Spaces - user can select the Space
    this.route('data', function() {
      // onedata/data/:space_id - show dir tree of a root dir of selected Space
      this.route('data-space', {path: '/:data_space_id'}, function() {
        // onedata/data/:space_id/:dir_id - a directory, which content is displayed
        //   currently in browser (and is expanded in data-files-tree)
        this.route('dir', {path: '/:dir_id'}, function() {
          // onedata/data/:space_id/:dir_id/:file_id - file selected in a browser
          this.route('file', {path: '/:file_id'});
        });

      });
    });

    // onedata/groups/ - all groups configuration reached from primary sidebar
    this.route('groups', function() {
      // onedata/groups/:group_id - entry for configuration of the single space
      this.route('show', {path: ':group_id'}, function() {
        // onedata/groups/:group_id/members - configure users/groups (members) permissions for space
        this.route('members');
      });
    });

    // onedata/shares/ - browse Shares reached from primary sidebar
    this.route('shares', function() {
        // onedata/shares/:share_id - show authenticated view for specified Share
        this.route('show', {path: ':share_id'});
    });

    // TODO: activate after routes implementation
    // this.route('recent', {resetNamespace: true});
    // this.route('links', {resetNamespace: true});
    // this.route('trash', {resetNamespace: true});

  });

  // /public - a root of all views that does not use authentication (public)
  this.route('public', {resetNamespace: true}, function() {

    // nothing here, TODO: redirect to index or something
    this.route('shares', {resetNamespace: true}, function() {
      // public/shares/:share_id - show public view of Share
      this.route('show', {path: ':share_id'});
    });

  });

  // FIXME this should redirect to OZ login page
  // unauthenticated only
  this.route('login');

  // handle routes not handled above and langauge-prefixed paths (e.g. /en/spaces)
  this.route('wildcard', { path: "*path"});
});

export default Router;
