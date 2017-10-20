import Ember from 'ember';

const {
  Component,
} = Ember;

export default Component.extend({
  /**
   * @virtual
   * FIXME: each provider on map info
   */
  providers: undefined,
  
  /**
   * @virtual
   * FIXME: provide collection of { src, dest } to create lines on map
   * only one for pair!
   */
  providerTransferConnections: undefined,
});
