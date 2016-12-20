import Ember from 'ember';

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

  bootstrapize: function() {
    if (this.get('title')) {
      this.$().tooltip('hide')
          .attr('data-original-title', this.get('title'))
          .tooltip('fixTitle');
    }
  }.observes('title'),

  didInsertElement() {
    this.bootstrapize();
  }
});
