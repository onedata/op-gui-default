import { expect } from 'chai';
import { describe, it } from 'mocha';
import ArraySlice from 'op-worker-gui/utils/array-slice';
import Ember from 'ember';
import _ from 'lodash';
import wait from 'ember-test-helpers/wait';
import sinon from 'sinon';

const {
  A,
  Object: EmberObject,
  computed,
} = Ember;

const ArraySum = EmberObject.extend({
  spy: undefined,
  as: undefined,
  sum: computed('as.[]', function () {
    this.get('spy')();
    return _.sum(this.get('as').toArray());
  }),
});

describe('Unit | Utility | array slice', function() {
  it('exposes array containing slice of original array', function() {
    const sourceArray = A(_.range(0, 100));
    
    const startIndex = 50;
    const endIndex = 70;
    const indexMargin = 10;
    
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    expect(
      as.toArray(),
      'should be slice of source array from 40 to 80'
    ).to.deep.equal(_.range(40, 80));
  });
  
  it('changes array contents when requested indexes change', function() {
    const sourceArray = A(_.range(0, 100));
    const startIndex = 50;
    const endIndex = 70;
    const indexMargin = 10;
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    as.setProperties({
      startIndex: 30,
      endIndex: 35,
    });
    
    const nativeArr = as.toArray();
    return wait().then(() => {
      expect(
        nativeArr,
        `${JSON.stringify(nativeArr)} should be array from 20 to 45`
      ).to.deep.equal(_.range(20, 45));
    });
  });
  
  it('allows to iterate on it with forEach', function() {
    const sourceArray = A(_.range(0, 100));
    const startIndex = 50;
    const endIndex = 70;
    const indexMargin = 10;
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    let j = 0;
    as.forEach(() => j++);
    expect(j).to.equal(40);
  });
  
  it('delegates pushObject to sourceArray', function() {
    const sourceArray = A(_.range(0, 100));
    const startIndex = 50;
    const endIndex = 70;
    const indexMargin = 10;
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    as.pushObject('x');
        
    expect(
      as.toArray(),
      'should be still a slice of source array from 40 to 80'
    ).to.deep.equal(_.range(40, 80));
    
    as.setProperties({
      indexMargin: 1,
      startIndex: 100,
      endIndex: 101,
    });
    
    return wait().then(() => {
      const nativeArr = as.toArray();
      expect(
        nativeArr,
        `${JSON.stringify(nativeArr)} should contain pushed object`
      ).to.deep.equal([99, 'x']);
    });
  });
  
  it('does not notify about changes in sourceArray if index is out of range', function() {
    const sourceArray = A(_.range(0, 100));
    const startIndex = 0;
    const endIndex = 5;
    const indexMargin = 1;
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    const spy = sinon.spy();
    
    const obj = EmberObject.extend({
      as,
      sum: computed('as.[]', function () {
        spy();
        return _.sum(this.get('as').toArray());
      }),
    }).create();
    
    expect(obj.get('sum')).to.equal(15);
    
    as.pushObject(10000);
    
    return wait().then(() => {
      expect(obj.get('sum')).to.equal(15);
      expect(spy).to.be.calledOnce;
    });
  });
  
  it('notifies about changes in sourceArray if index is in range', function() {
    const sourceArray = A(_.concat([99, 99, 99], _.range(0, 6)));
    const startIndex = 3;
    const endIndex = 10;
    const indexMargin = 0;
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    const spy = sinon.spy();
    
    const obj = EmberObject.extend({
      as,
      sum: computed('as.[]', function () {
        spy();
        return _.sum(this.get('as').toArray());
      }),
    }).create();
    
    expect(obj.get('sum')).to.equal(15);
    
    as.pushObject(10000);
    
    return wait().then(() => {
      expect(obj.get('sum')).to.equal(10015);
      expect(spy).to.be.calledTwice;
    });
  });
  
  it('notifies about changes in array if increasing the endIndex', function () {
    const sourceArray = A(_.concat(_.range(0, 10)));
    const startIndex = 0;
    const endIndex = 3;
    const indexMargin = 0;
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    const spy = sinon.spy();
    
    const obj = ArraySum.create({
      as,
      spy,
    });
    
    expect(obj.get('sum')).to.equal(_.sum([0, 1, 2]));
    
    as.set('endIndex', 5);
    
    return wait().then(() => {
      const newSum = obj.get('sum');
      expect(spy).to.be.calledTwice;
      expect(newSum).to.equal(_.sum(_.range(0, 5)));
    });
  });
  
  it('notifies about changes in array if decreasing the endIndex', function () {
    const sourceArray = A(_.concat(_.range(0, 10)));
    const startIndex = 0;
    const endIndex = 5;
    const indexMargin = 0;
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    const spy = sinon.spy();
    
    const obj = ArraySum.create({
      as,
      spy,
    });
    
    expect(obj.get('sum')).to.equal(_.sum(_.range(0, 5)));
    
    as.set('endIndex', 3);
    
    return wait().then(() => {
      const newSum = obj.get('sum');
      expect(spy).to.be.calledTwice;
      expect(newSum).to.equal(_.sum(_.range(0, 3)));
    });
  });
  
  it('notifies about changes in array if decreasing the startIndex', function () {
    const sourceArray = A(_.concat(_.range(0, 10)));
    const startIndex = 7;
    const endIndex = 9;
    const indexMargin = 0;
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    const spy = sinon.spy();
    
    const obj = ArraySum.create({
      as,
      spy,
    });
    
    expect(obj.get('sum')).to.equal(_.sum(_.range(7, 9)));
    
    as.set('startIndex', 5);
    
    return wait().then(() => {
      const newSum = obj.get('sum');
      expect(spy).to.be.calledTwice;
      expect(newSum).to.equal(_.sum(_.range(5, 9)));
    });
  });
  
  it('notifies about changes in array if increasing the startIndex', function () {
    const sourceArray = A(_.concat(_.range(0, 10)));
    const startIndex = 7;
    const endIndex = 10;
    const indexMargin = 0;
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    const spy = sinon.spy();
    
    const obj = ArraySum.create({
      as,
      spy,
    });
    
    expect(obj.get('sum')).to.equal(_.sum(_.range(7, 10)));
    
    as.set('startIndex', 8);
    
    return wait().then(() => {
      const newSum = obj.get('sum');
      expect(spy).to.be.calledTwice;
      expect(newSum).to.equal(_.sum(_.range(8, 10)));
    });
  });
  
  it('notifies about changes in array if changing the indexMargin', function () {
    const sourceArray = A(_.concat(_.range(0, 100)));
    const startIndex = 20;
    const endIndex = 25;
    const indexMargin = 10;
    const as = ArraySlice.create({
      sourceArray,
      startIndex,
      endIndex,
      indexMargin,
    });
    
    const spy = sinon.spy();
    
    const obj = ArraySum.create({
      as,
      spy,
    });
    
    expect(obj.get('sum'), '10..35').to.equal(_.sum(_.range(10, 35)));
    
    as.set('indexMargin', 5);
    
    return wait().then(() => {
      const newSum = obj.get('sum');
      return wait().then(() => {
        expect(spy).to.be.calledTwice;
        expect(newSum, '15..30').to.equal(_.sum(_.range(15, 30)));
      });
    });
  });
});
