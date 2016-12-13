import Ember from 'ember';

export default Ember.Service.extend({
  secondsToRedirect: null,

  init() {
    this._super(...arguments);
    this.clearTimeouts();
  },

  startTimeout() {
    const rid = setTimeout(function() {
      window.location = "/login.html";
    }, this.get('secondsToRedirect')*1000);
    this.get('timeoutIds').pushObject(rid);
  },

  clearTimeouts() {
    const tids = this.get('timeoutIds');
    if (tids) {
      tids.forEach(tid => {
        window.clearTimeout(tid);
      });
    }
    
    this.set('timeoutIds', Ember.A());
  }
});
