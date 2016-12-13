/* jshint expr:true */
import { expect } from 'chai';
import sinon from 'sinon';
import sinonChai from 'ember-cli-onedata-common/exports/sinon-chai';
import chai from 'chai';
chai.use(sinonChai);

import {
  describeModule,
  it
} from 'ember-mocha';

describeModule(
  'service:server-messages-handler',
  'ServerMessagesHandlerService',
  {
    // Specify the other units that are required for this test.
    // needs: ['service:foo']
  },
  function() {
    // Replace this with your real tests.
    it('exists', function() {
      let service = this.subject();
      expect(service).to.be.ok;
    });

    it('allows to register an event handler which will be triggered on event', function() {
      const service = this.subject();
      const eventName = 'hello-world';
      const handlerSpy = sinon.spy();
      const data = {hello: 'world', one: 'data'};

      service.onMessage(eventName, handlerSpy);
      service.triggerEvent(eventName, data);

      expect(handlerSpy).to.have.been.calledWith(data);
    });
  }
);
