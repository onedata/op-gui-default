import Ember from 'ember';

const latestControllerMixin = Ember.Mixin.create({
  latestRoute: Ember.inject.service(),
  isActive: Ember.computed('latestRoute.controller', function() {
    const latestController = this.get('latestRoute.controller');
    return latestController === this;
  }),
});

const latestRouteMixin = Ember.Mixin.create({
  latestRoute: Ember.inject.service(),
  activate() {
    this._super(...arguments);
    this.set('latestRoute.route', this);
  }
});

export function initialize(application) {
  application.inject('route', 'latest-route', 'service:latest-route');
  application.inject('controller', 'latest-route', 'service:latest-route');

  Ember.Controller.reopen(latestControllerMixin);
  Ember.Route.reopen(latestRouteMixin);
}

export default {
  name: 'latest-route',
  initialize
};
