import { expect } from 'chai';
import { describe, it, beforeEach } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import { registerService } from '../../../helpers/stub-service';
import Ember from 'ember';

const {
  Service,
  RSVP: { defer },
} = Ember;

const providerName = 'My provider name';

const I18n = Service.extend({
  t() {}
});

const Session = Service.extend({
  sessionDetails: {
    providerName,
  },
});

describe('Integration | Component | modals/provider redirect', function () {
  setupComponentTest('modals/provider-redirect', {
    integration: true
  });

  beforeEach(function () {
    registerService(this, 'i18n', I18n);
    registerService(this, 'session', Session);
  });

  it('resolves chosen provider from list', function (done) {
    const deferredProviderChoice = defer();
    const alpha = {
      id: 'id2',
      name: 'Alpha',
      online: false,
    };
    const zeta = {
      id: 'id1',
      name: 'Zeta',
      online: true,
    };
    const beta = {
      id: 'id3',
      name: 'Beta',
      online: true,
    };
    const providers = [zeta, alpha, beta];

    this.setProperties({
      deferredProviderChoice,
      providers,
    });

    this.render(hbs `{{modals/provider-redirect
      open=true
      deferredProviderChoice=deferredProviderChoice
      providers=providers
    }}`);

    setTimeout(() => {
      this.$('.select2-container a').trigger({ type: 'mousedown', which: 1 });
      setTimeout(() => {
        $('.select2-results li div').trigger({
          type: 'mouseup',
          which: 1
        });
        deferredProviderChoice.promise.then(provider => {
          expect(provider).to.be.equal(beta);
          done();
        });
      }, 0);
    }, 100);

  });
});
