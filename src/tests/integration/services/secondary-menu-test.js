/* jshint expr:true */
import { expect } from 'chai';
import {
  describeModule,
  it
} from 'ember-mocha';
import {
  beforeEach,
  afterEach
} from 'mocha';

import startApp from '../../helpers/start-app';

import Ember from 'ember';

describeModule(
  'service:secondary-menu',
  'Integration: SecondaryMenuService',
  {
    integration: true
  },
  function() {
    beforeEach(function () {
      this.application = startApp();
      this.store = this.application.__container__.lookup('service:store');
    });

    afterEach(function() {
      Ember.run(this.application, 'destroy');
    });

    it('exists', function() {
      let service = this.subject();
      expect(service).to.be.ok;
    });

    it('returns activeGroup if activeItem is of Group type', function () {
      let g = this.store.createRecord('group', {id: 'g1'});

      this.subject().set('activeItem', g);

      expect(this.subject().get('activeGroup')).to.not.be.null;
      expect(this.subject().get('activeGroup.id')).to.equal('g1');
    });

    it('returns null activeSpace if activeItem is of Group type', function () {
      let g = this.store.createRecord('group', {
        id: 'g1',
        name: 'hello'
      });

      this.subject().set('activeItem', g);

      expect(this.subject().get('activeSpace')).to.be.null;
    });

    it('returns activeSpace if activeItem is of Space type', function () {
      let g = this.store.createRecord('space', {id: 's1'});

      this.subject().set('activeItem', g);

      expect(this.subject().get('activeSpace')).to.not.be.null;
      expect(this.subject().get('activeSpace.id')).to.equal('s1');
    });

    it('returns null activeGroup if activeItem is of Space type', function () {
      let s = this.store.createRecord('space', {id: 's1'});

      this.subject().set('activeItem', s);

      expect(this.subject().get('activeGroup')).to.be.null;
    });
  }
);
