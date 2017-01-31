export function initialize(application) {
  ['route', 'component', 'controller'].forEach(name => {
    application.inject(name, 'eventsBus', 'service:events-bus');
  });
}

export default {
  name: 'events-bus',
  initialize: initialize
};