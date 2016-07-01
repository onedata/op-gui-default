import Ember from 'ember';

export default Ember.Component.extend({
  /**
   * @type FileAccessList[]
   */
  acl: Ember.computed.alias('file.acl'),
});
