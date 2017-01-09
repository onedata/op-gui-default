/* jshint expr:true */
import { expect } from 'chai';
import { it, describe } from 'mocha';
import { setupComponentTest } from 'ember-mocha';

describe('StatusPanelComponent', function() {
  setupComponentTest('status-panel', {
    // Specify the other units that are required for this test
    needs: ['component:spin-spinner',
      'component:modals/elements/alert-panel',
      'helper:eq', 'helper:and', 'helper:or'],
    unit: true
  });

  it('renders', function() {
    // creates the component instance
    let component = this.subject();
    // renders the component on the page
    this.render();
    expect(component).to.be.ok;
    expect(this.$()).to.have.length(1);
  });

  it('displays message if blocking is true', function() {
    let comp = this.subject();
    comp.setProperties({
      blocking: true,
      type: 'info',
      message: 'hello world 1'
    });
    this.render();

    expect(this.$(), `html: ${this.$().html()}`)
      .to.contain('hello world 1');
  });

  it('displays message if blocking is false', function() {
    let comp = this.subject();
    comp.setProperties({
      blocking: false,
      type: 'info',
      message: 'hello world 2'
    });
    this.render();

    expect(this.$(), `html: ${this.$().html()}`)
      .to.contain('hello world 2');
  });

  it('does not display message if type is null', function() {
    let comp = this.subject();
    comp.setProperties({
      blocking: true,
      type: null,
      message: 'hello world 3'
    });
    this.render();

    expect(this.$()).not.to.contain('hello world 3');
  });

  it('does not display status-panel-inner if message is null', function() {
    let comp = this.subject();
    comp.setProperties({
      blocking: true,
      type: 'info',
      message: null
    });
    this.render();

    expect(this.$('.status-panel-inner')).not.to.exist;
  });
});
