/* jshint esversion: 6 */

const flatten = require('flat');

const printTranslations = function(enTranlation) {
  console.log(JSON.stringify({keys: Object.keys(flatten(enTranlation.default))}));
};

const enTranslation = require('./locales/en/translations');

printTranslations(enTranslation);
