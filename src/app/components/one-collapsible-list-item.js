import Ember from 'ember';
const {
  computed,
  computed: {
    notEmpty,
    reads,
  },
  observer,
  inject: {
    service,
  },
} = Ember;

/**
 * Item class of the collapsible list. For example of use case see 
 * components/one-collapsible-list.js.
 * 
 * If isCollapsible == false then item cannot be toggled.
 * Item closes its content if eventsBus triggers closeEventName event
 * 
 * Module imported from onedata-gui-common.
 *
 * @module components/one-collapsible-list-item.js
 * @author Michał Borzęcki
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default Ember.Component.extend({
  tagName: 'li',
  classNames: ['one-collapsible-list-item', 'collapse-animation', 'collapse-medium'],
  classNameBindings: [
    'isActive:active',
    '_isItemCollapsed:collapse-hidden',
    '_isSelected:selected'
  ],

  eventsBus: service(),

  /**
   * Notify parent about item open/close.
   * Note: you should know if this is an accordion or multi-collapsible-list
   * @virtual optional
   * @type {Function} `(opened: boolean) => any`
   */
  itemToggled: () => {},
  
  isCollapsible: true,
  accordionMode: false,
  activeElementId: '',
  closeEventName: null,

  /**
   * Value, that will be returned by one-collapsible-list on this item select
   * @type {*}
   */
  selectionValue: null,

  /**
   * Item selection change handler. Injected by one-collapsible-list.
   * @type {Function}
   */
  toggleItemSelection: () => {},

  toggle: () => {},

  /**
   * Can be injected to take external control on item collapse state.
   * If not injected, it is initialized to `reads` of `_isActive` in `init`.
   * @type {Ember.ComputedProperty<boolean>|boolean}
   */
  isActive: undefined,
  
  /**
   * List of selected list items
   * @type {Array.*}
   */
  _selectedItemValues: [],

  /**
   * If true, item has a checkbox
   * @type {boolean}
   */
  _hasCheckbox: false,

  /**
   * Item value notification handler. Sends to parent value of this item.
   * @type {Function}
   */
  _notifyValue: () => {},

  /**
   * If true, list is collapsed
   * @type {boolean}
   */
  _isListCollapsed: false,

  /**
   * Search query
   * @type {string}
   */
  _searchQuery: '',

  /**
   * If true, list item matches searched text
   * @type {boolean}
   */
  _matchesSearchQuery: true,

  _isItemCollapsed: computed('_isListCollapsed', '_matchesSearchQuery',
    '_isSelected',
    function () {
      let {
        _isListCollapsed,
        _matchesSearchQuery,
        _isSelected
      } = this.getProperties(
        '_isListCollapsed',
        '_matchesSearchQuery',
        '_isSelected'
      );
      return _isListCollapsed || (!_matchesSearchQuery && !_isSelected);
    }
  ),

  _isItemFixed: computed('_matchesSearchQuery', '_isSelected', function () {
    let {
      _matchesSearchQuery,
      _isSelected
    } = this.getProperties('_matchesSearchQuery', '_isSelected');
    return !_matchesSearchQuery && _isSelected;
  }),

  _isActive: computed('activeElementId', 'accordionMode', function () {
    let {
      activeElementId,
      elementId
    } = this.getProperties([
      'activeElementId', 'elementId'
    ]);
    if (this.get('accordionMode')) {
      return activeElementId === elementId;
    }
  }),

  _isSelected: computed('_selectedItemValues.[]', 'selectionValue', function () {
    let {
      _selectedItemValues,
      selectionValue,
    } = this.getProperties('_selectedItemValues', 'selectionValue');
    return _selectedItemValues.indexOf(selectionValue) > -1;
  }),

  _isCheckboxActive: notEmpty('selectionValue'),

  _searchQueryObserver: observer('_searchQuery', function () {
    this._checkSearchQuery();
  }),

  _matchesSearchQueryAndIsSelectedObserver: observer('_matchesSearchQuery',
    '_isSelected',
    function () {
      let {
        _matchesSearchQuery,
        _isSelected,
        selectionValue,
        _notifyValue
      } = this.getProperties(
        '_matchesSearchQuery',
        '_isSelected',
        'selectionValue',
        '_notifyValue'
      );
      // Add/remove item value from list after filter
      if (selectionValue !== null) {
        if (!_matchesSearchQuery && !_isSelected) {
          _notifyValue(selectionValue, false);
        } else if (_matchesSearchQuery) {
          _notifyValue(selectionValue, true);
        }
      }
    }
  ),

  init() {
    this._super(...arguments);
    let {
      closeEventName,
      eventsBus,
      selectionValue
    } = this.getProperties('closeEventName', 'eventsBus', 'selectionValue');
    if (closeEventName) {
      eventsBus.on(closeEventName, () => this.set('_isActive', false));
    }
    if (selectionValue !== null) {
      this.get('_notifyValue')(selectionValue, true);
    }
    const isActive = this.get('isActive');
    if (isActive) {
      this.set('_isActive', isActive);
    } else {
      this.isActive = reads('_isActive');
    }
  },

  willDestroyElement() {
    try {
      let selectionValue = this.get('selectionValue');
      if (selectionValue !== null) {
        this.get('_notifyValue')(selectionValue, false);
      }
    } finally {
      this._super(...arguments);
    }
  },

  _checkSearchQuery() {
    let {
      _searchQuery,
      _matchesSearchQuery,
    } = this.getProperties('_searchQuery', '_matchesSearchQuery');
    let headerTextElement = this.$('.one-collapsible-list-item-header');
    let oneLabel = headerTextElement.find('.one-label');
    let targetElement = oneLabel.length ? oneLabel : headerTextElement;
    let matches = targetElement.text().toLowerCase()
      .search(_searchQuery.trim().toLowerCase()) > -1;
    if (matches !== _matchesSearchQuery && !matches) {
      this.send('toggle', false);
    }
    this.set('_matchesSearchQuery', matches);
  },

  actions: {
    toggle(opened) {
      let newOpenedState = opened;
      if (!this.get('isCollapsible')) {
        return;
      }
      if (this.get('accordionMode')) {
        this.get('toggle')(this.get('elementId'), opened);
      } else {
        if (opened !== undefined) {
          this.set('_isActive', !!opened);
        } else {
          newOpenedState = this.toggleProperty('_isActive');
        }
      }
      this.get('itemToggled')(newOpenedState);
    },
    toggleSelection() {
      this('toggleItemSelection')(this.get('selectionValue'));
    }
  }
});
