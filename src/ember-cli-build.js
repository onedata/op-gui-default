/*jshint node:true*/
/* global require, module */
var EmberApp = require('ember-cli/lib/broccoli/ember-app');
var fs = require('fs');

module.exports = function(defaults) {
  var app = new EmberApp(defaults, {
    sassOptions: {
      includePaths: [
        'app/styles',
        'app/styles/components',
        'app/styles/media-styles',
        'app/styles/media-variables',
        // emer-cli-onedata-common addon
        'lib/ember-cli-onedata-common/app/styles',
        'lib/ember-cli-onedata-common/app/styles/oneicons',
        'lib/ember-cli-onedata-common/app/styles/components',
      ]
    },
    // a "bootstrap" should be imported into app.scss
    'ember-cli-bootstrap-sassy': {
      // import SASS styles and some JS that is used outside of ember-bootstrap components 
      'js': [
        // TODO: use ember-bootstrap tooltip (needs refactoring and removing own bs-tooltip component)
        'tooltip',
        'transition',
        // TODO: rewrite collapses to ember-bootstrap components
        'collapse',
        // TODO: use bs-alert inside alert-panel component
        'alert',
        // TODO: rewrite dropdowns to ember-bootstrap components
        'dropdown'
      ],
      'glyphicons': false
    },
    // import only JS
    'ember-bootstrap': {
      'importBootstrapCSS': false,
      'importBootstrapTheme': false,
      'importBootstrapFont': false
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
