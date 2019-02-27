import Ember from 'ember';

const {
  Route,
} = Ember;

export default Route.extend({
  renderTemplate() {
    this.render('disabled', { outlet: 'data-space-sidebar' });
    this.render('onedata/-space-content-error', { outlet: 'data-content-scroll' });
    this.render('disabled', {
      into: 'onedata',
      outlet: 'toolbar'
    });
  },
});
