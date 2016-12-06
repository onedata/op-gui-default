/*jshint node:true*/
/* global require, module */
var EmberApp = require('ember-cli/lib/broccoli/ember-app');
var EmberAddon = require('ember-cli/lib/broccoli/ember-addon');
var fs = require('fs');

module.exports = function(defaults) {
  var app = new EmberApp(defaults, {
    sassOptions: {
      includePaths: ['app/styles', 'app/styles/media-styles',
        'app/styles/media-variables', 'app/styles/components']
    },
    'ember-bootstrap': {
      'importBootstrapFont': false,
      'importBootstrapCSS': false,
      'importBootstrapTheme': false,
    }
  });

  // Generate app-config.json for environment that is used.
  // Currently app-config.json is always overwritten on build.
  var onedataAppConfig = {
    debug: !app.isProduction
  };
  fs.writeFile("public/app-config.json", JSON.stringify(onedataAppConfig), function(err) {
    if (err) {
      return console.error('Error on writing app-config.json: ' + err);
    }
  });

  // Use `app.import` to add additional libraries to the generated
  // output files.
  //
  // If you need to use different assets in different
  // environments, specify an object as the first parameter. That
  // object's keys should be the environment name and the values
  // should be the asset to use in that environment.
  //
  // If the library that you are including contains AMD or ES6
  // modules that you would like to import into your application
  // please specify an object with the list of modules as keys
  // along with the exports of each module as its value.

  const BOWER_ASSETS = [
    'resumable.js/resumable.js',
    'moment/min/moment-with-locales.min.js',
    'spin.js/spin.js',
    'animate.css/animate.min.css',
    'jquery-resizable/dist/jquery-resizable.min.js',
    'df-visible/jquery.visible.min.js',
    'jquery-sticky/jquery.sticky.js',
  ];

  BOWER_ASSETS.forEach(path => app.import(app.bowerDirectory + '/' + path));

  return app.toTree();
};
