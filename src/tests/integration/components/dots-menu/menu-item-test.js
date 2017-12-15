import { expect } from 'chai';
import { describe, it } from 'mocha';
import { setupComponentTest } from 'ember-mocha';
import hbs from 'htmlbars-inline-precompile';
import wait from 'ember-test-helpers/wait';
import sinon from 'sinon';

describe('Integration | Component | dots menu/menu item', function() {
  setupComponentTest('dots-menu/menu-item', {
    integration: true
  });

  it('renders li element with icon and label', function() {
    this.render(hbs`{{dots-menu/menu-item
      icon="someicon"
      title="some title"
    }}`);
    const $menuItem = this.$('li.dots-menu-item');
    
    expect($menuItem, 'item')
      .to.exist;
    expect($menuItem.find('.oneicon-someicon'), 'icon')
      .to.exist;
    expect($menuItem.find('.item-label').text(), 'label')
      .to.contain('some title');
  });
  
  it('invoked passed action on click', function(done) {
    const spy = sinon.spy();
    this.on('testAction', spy);
    
    this.render(hbs`{{dots-menu/menu-item
      icon="someicon"
      title="some title"
      action=(action "testAction")
    }}`);
    
    const $menuItem = this.$('li.dots-menu-item');
    $menuItem.click();
    
    wait().then(() => {
      expect(spy).to.be.calledOnce;
      done();
    });
  });
  
  it('renders as disabled and cannot be clicked if disabled', function(done) {
    const spy = sinon.spy();
    this.on('testAction', spy);
    
    this.render(hbs`{{dots-menu/menu-item
      icon="someicon"
      title="some title"
      action=(action "testAction")
      disabled=true
    }}`);
    
    const $menuItem = this.$('li.dots-menu-item');
    expect($menuItem).to.have.class('disabled');
    $menuItem.click();
    
    wait().then(() => {
      expect(spy).to.not.be.called;
      done();
    });
  });
});
