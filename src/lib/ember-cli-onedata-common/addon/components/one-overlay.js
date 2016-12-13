import Ember from 'ember';
import layout from '../templates/components/one-overlay';

export default Ember.Component.extend({
  layout,

  didInsertElement() {
    let $parent = $().parent();
    // let $child = $('.overlay-content');
    if ($parent.css('position') === 'static') {
      $parent.css('position', 'relative');
    }
    $().css({
      position: 'absolute',
      top: 0,
      right: 0,
      bottom: 0,
      left: 0,
      maxHeight: '100%',
      maxWidth: '100%'
    });
  }
});
