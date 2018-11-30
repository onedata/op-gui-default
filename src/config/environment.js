/* jshint node: true */

module.exports = function(environment) {
  var ENV = {
    modulePrefix: 'op-worker-gui',
    environment: environment,
    rootURL: null,
    // set has location, because we want to have uris to share
    // without History API
    // see: http://emberjs.com/api/classes/Ember.Location.html
    locationType: 'hash',
    EmberENV: {
      FEATURES: {
        // Here you can enable experimental features on an ember canary build
        // e.g. 'with-controller': true
      }
    },

    browserify: {
      // your browserify options if you have any
      ignores: [ ]
    },

    APP: {
      // Here you can pass flags/options to your application instance
      // when it is created
    },

    i18n: {
      defaultLocale: 'en'
    },

    'ember-simple-auth': {
      session: 'session:custom',
      authenticationRoute: 'login', // 'login'
      routeAfterAuthentication: 'onedata', // 'onedata'
      routeIfAlreadyAuthenticated: 'onedata' // 'onedata'
    },
    
    webFontConfig: {
      custom: {
        families: ['Open Sans'],
      },
      timeout: 60000,
    },
  };

  if (['development', 'localstorage'].indexOf(environment) !== -1) {
    // ENV.APP.LOG_RESOLVER = true;
     ENV.APP.LOG_ACTIVE_GENERATION = true;
     ENV.APP.LOG_TRANSITIONS = true;
    // ENV.APP.LOG_TRANSITIONS_INTERNAL = true;
    // ENV.APP.LOG_VIEW_LOOKUPS = true;
  }

  if (environment === 'test') {
    // Testem prefers this...
    ENV.locationType = 'none';

    // keep test console output quieter
    ENV.APP.LOG_ACTIVE_GENERATION = false;
    ENV.APP.LOG_VIEW_LOOKUPS = false;

    ENV.APP.rootElement = '#ember-testing';
  }

  if (environment === 'production') {
    ENV.browserify.ignores.push('sinon-chai');
    ENV.browserify.ignores.push('chai-jquery');
  }

  return ENV;
};
