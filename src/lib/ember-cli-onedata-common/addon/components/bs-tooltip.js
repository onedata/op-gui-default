import Ember from 'ember';

const {
  observer,
  run
} = Ember;

/**
 * Adds a Bootstrap tooltip on yielded content.
 * Example of usage:
 * ```
 * {{#bs-tooltip tagName="p" title="Extended info" placement="left"}}
 *   Some text
 * {{/bs-tooltip}}
 * ```
 * The default placement is "bottom".
 * @todo remove this component and use ember-bootstrap tooltip component (needs projects refactoring)
 * @module components/bs-tooltip
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  // does not have a template
  // layout,

  attributeBindings: ['dataToggle:data-toggle', 'placement:data-placement'],
  dataToggle: 'tooltip',
  // default
  placement: 'bottom',
  title: null,

  bootstrapize: observer('title', function() {
    if (this.get('title')) {
      run.scheduleOnce('afterRender', this, function() {
        this.$().tooltip('hide')
            .attr('data-original-title', this.get('title'))
            .tooltip('fixTitle');
      });
    }
  }),

  didInsertElement() {
    this.bootstrapize();
  }
});
