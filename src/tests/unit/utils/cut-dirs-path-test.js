/* jshint expr:true */
import Ember from 'ember';
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import cutDirsPath from 'op-worker-gui/utils/cut-dirs-path';

describe('cutDirsPath', function() {
  it('returns modified dirsPath with rootDir as first element', function () {
    const f0 = Ember.Object.create({
      id: '0',
      parent: null,
    });
    const f1 = Ember.Object.create({
      id: '1',
      parent: f0,
    });
    const f2 = Ember.Object.create({
      id: '2',
      parent: f1,
    });
    const f3 = Ember.Object.create({
      id: '3',
      parent: f2,
    });
    const f4 = Ember.Object.create({
      id: '4',
      parent: f3,
    });

    const dirsPath = [f0, f1, f2, f3, f4];

    const result = cutDirsPath(dirsPath, f2);

    expect(result[0].get('id')).to.be.equal(f2.get('id'));
    expect(result[1].get('id')).to.be.equal(f3.get('id'));
    expect(result[2].get('id')).to.be.equal(f4.get('id'));
  });
});
