import Ember from 'ember';
import ACL from '../../utils/access-control-entity';

export default Ember.Component.extend({
  store: Ember.inject.service(),

  file: null,

  /**
   * Should be injected.
   * @type FileAccessList[]
   */
  acl: Ember.computed.alias('file.acl'),

  // FIXME: test code, using fake ACL
  // acl: this.get('store').createRecord('fileAcl', {
  //   file: this.get('file'),
  //   acl: [
  //     ACL.create({
  //       type: 'deny',
  //       subject: 'everyone',
  //       permissions: 2
  //     }),
  //     ACL.create({
  //       type: 'allow',
  //       subject: 'owner',
  //       permissions: 4
  //     }),
  //   ]
  // }),

  actions: {
    // TODO: just for tests
    addAc() {
      const r = this.get('store').createRecord('fileAcl', {
        file: this.get('file'),
        acl: [
          ACL.create({
            type: 'deny',
            subject: 'everyone',
            permissions: 0,
            user: null,
            group: null,
          }).toJSON()
        ]
      });
      r.save().catch(() => {
        // debugger;
      });
    }
  }
});
