/* jshint expr:true */
import { expect } from 'chai';
import sinon from 'sinon';
import sinonChai from 'ember-cli-onedata-common/exports/sinon-chai';
import chai from 'chai';
chai.use(sinonChai);

import Ember from 'ember';

import {
  setupTest,
} from 'ember-mocha';

import {
  it,
  describe
} from 'mocha';

describe('EventsBusService', function() {
    setupTest('service:events-bus', {
      // Specify the other units that are required for this test.
      // needs: ['service:foo']
    });
    
    const Sender = Ember.Object.extend({
      sendGlobalEvent(a, b) {
        this.get('eventsBus').trigger('sender:foo', a, b);
      }
    });
    const Receiver = Ember.Object.extend({
      init() {
        this._super(...arguments);
        let __eventHandlerFun = (a, b) => this.eventHandler(a, b);
        this.get('eventsBus').on('sender:foo', __eventHandlerFun);
        this.set('__eventHandlerFun', __eventHandlerFun);
      },
      eventHandler() {
      },
      disableEventHandler() {
        this.get('eventsBus').off('sender:foo', this.get('__eventHandlerFun'));
      }
    });

    it('allows to bind a global event handler between two ember objects', function() {
      let eventsBus = this.subject();
      let sender = Sender.create({eventsBus: eventsBus});
      let receiver = Receiver.create({eventsBus: eventsBus});
      let handlerSpy = sinon.spy(receiver, 'eventHandler');

      sender.sendGlobalEvent('a', 'b');

      expect(handlerSpy).to.have.been.calledWith('a', 'b');
    });

    it('allows to disable a event handler', function() {
      let eventsBus = this.subject();
      let sender = Sender.create({eventsBus: eventsBus});
      let receiver = Receiver.create({eventsBus: eventsBus});
      let handlerSpy = sinon.spy(receiver, 'eventHandler');
      receiver.disableEventHandler();

      sender.sendGlobalEvent();

      expect(handlerSpy).to.have.not.been.called;
    });
  }
);
