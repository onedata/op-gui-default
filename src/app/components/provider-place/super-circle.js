// FIXME: jsdoc

import Ember from 'ember';

const {
  Component,
  computed,
  String: {
    htmlSafe,
  },
} = Ember;

export default Component.extend({
  classNames: ['super-circle'],
  classNameBindings: ['isSource:source', 'isDestination:destination'],
  attributeBindings: ['style'],
  
  /**
   * @type {boolean}
   */
  isSource: false,
  
  /**
   * @type {boolean}
   */
  isDestination: false,

  /**
   * @virtual
   * @type {string}
   */
  circleColor: '',

  style: computed('circleColor', function () {
    const circleColor = this.get('circleColor');
    return htmlSafe(`background-color: ${circleColor};`);
  }),
});
