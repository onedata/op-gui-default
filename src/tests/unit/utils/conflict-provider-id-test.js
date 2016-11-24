/* jshint expr:true */
import { expect } from 'chai';
import {
  describe,
  it
} from 'mocha';
import conflictProviderId from 'op-worker-gui/utils/conflict-provider-id';

describe('conflictProviderId', function() {
  it('generates ids with length greater than 3 if both ids are completely different', function() {
    let result = conflictProviderId([
      'onetwothree',
      'fourfivesix',
      'seveneightten'
    ]);

    expect(result.length).to.equal(3);

    let [resultA, resultB, resultC] = result;
    expect(resultA).to.equal('onet');
    expect(resultB).to.equal('four');
    expect(resultC).to.equal('seve');
  });

  it('generates ids with length of minimum different ids', function() {
    let result = conflictProviderId([
      'somestring111',
      'somestring211',
      'somestring311'
    ]);

    expect(result.length).to.equal(3);

    let [resultA, resultB, resultC] = result;
    expect(resultA).to.equal('somestring1');
    expect(resultB).to.equal('somestring2');
    expect(resultC).to.equal('somestring3');
  });

  it('generates ids maximum length of their original', function() {
    let result = conflictProviderId([
      'so',
      'somes211',
      'somestring311'
    ]);

    expect(result.length).to.equal(3);

    let [resultA, resultB, resultC] = result;
    expect(resultA).to.equal('so');
    expect(resultB).to.equal('somes2');
    expect(resultC).to.equal('somest');
  });

  it('generates ids with equal length for differen pair differences', function() {
    let result = conflictProviderId([
      'somest123',
      'somest456',
      'ringt789123',
      'ringt789abcd',
    ]);

    expect(result.length).to.equal(4);

    let [resultA, resultB, resultC, resultD] = result;
    expect(resultA).to.equal('somest123');
    expect(resultB).to.equal('somest456');
    expect(resultC).to.equal('ringt7891');
    expect(resultD).to.equal('ringt789a');
  });
});
