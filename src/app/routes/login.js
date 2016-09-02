import Ember from 'ember';

export default Ember.Route.extend({
  // FIXME redirect to OZ login page, get url from session
  activate() {
    setTimeout(function () {
      window.location = "https://veilfsdev.com/#/home/login";
    }, 5000);
  }
});
