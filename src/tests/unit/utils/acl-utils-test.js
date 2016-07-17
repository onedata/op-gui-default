/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import { mergeAcls } from 'op-worker-gui/utils/acl-utils';
import ACE from '../../../utils/access-control-entity';

describe('aclUtils', function() {
  it('has mergeAcls function which creates one AccessControlEntity (ACE) from an ACE[] without duplicate elements', function() {
    const ace1 = ACE.create({permissions: 0});
    ace1.setPermission('read_object');

    const ace2 = ACE.create({permissions: 0});
    ace2.setPermission('read_object');

    const ace3 = ACE.create({permissions: 0});
    ace3.setPermission('write_object');

    const result = mergeAcls([[ace1], [ace2, ace3]]);

    expect(result.length).to.be.equal(2);

    const newAce1 = result[0];
    const newAce2 = result[1];

    expect(
      newAce1.hasPermission('read_object') && !newAce2.hasPermission('read_object') ||
      !newAce1.hasPermission('read_object') && newAce2.hasPermission('read_object')
    ).to.be.true;
  });
});
