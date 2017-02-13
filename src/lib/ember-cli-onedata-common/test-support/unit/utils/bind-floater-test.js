/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import bindFloater from 'ember-cli-onedata-common/utils/bind-floater';

describe('bindFloater', function() {
  it('exists as a function', function() {
    expect(typeof(bindFloater)).to.be.equal('function');
  });
});
