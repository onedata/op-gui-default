/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import snakeToCamel from 'op-worker-gui/utils/snake-to-camel';

describe('snakeToCamel', function() {
  it('converts snake-case to camelCase by default', function() {
    let result = snakeToCamel('hello-world');
    expect(result).to.be.equal('helloWorld');
  });

  it('converts snake_case to camelCase when using "_" separator', function() {
    let result = snakeToCamel('hello_world', '_');
    expect(result).to.be.equal('helloWorld');
  });
});
