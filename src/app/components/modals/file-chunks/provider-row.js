import Ember from 'ember';

const {
  Component,
  computed,
  get,
  isEmpty,
} = Ember;

export default Component.extend({
  tagName: 'tr',
  
  /**
   * @virtual
   * @type {FileDistribution}
   */
  fileDistribution: undefined,
  
  /**
   * @virtual
   * @type {boolean}
   */
  currentProviderSupport: undefined,
  
  /**
   * @virtual
   * @type {Ember.Array<Transfer>|undefined}
   */
  fileTransfers: undefined,
  
  /**
   * @virtual
   * @type {Function}
   */
  openMigrationOptions: () => {},
  
  /**
   * @virtual
   * @type {Function}
   */
  startReplication: () => {},

  /**
   * Css classes for pending action
   * @type {string}
   */
  pendingActionAnimation: 'in-progress animated infinite semi-hinge pulse-mint',

  // TODO: make these properties computed
  replicationInProgress: computed.equal('replicationType', 'replication'),
  replicationEnabled: computed(
    'neverSynchronized',
    'isComplete',
    'migrationInProgress',
    'currentProviderSupport',
    function () {
      const {
        neverSynchronized,
        isComplete,
        migrationInProgress,
        currentProviderSupport
      } = this.getProperties(
        'neverSynchronized',
        'isComplete',
        'migrationInProgress',
        'currentProviderSupport'
      );
      return currentProviderSupport && (neverSynchronized || !isComplete) &&
        !migrationInProgress;
    }
  ),
  
  migrationInProgress: computed.equal('replicationType', 'migration'),
  migrationEnabled: computed(
    'isEmpty',
    'migrationInProgress',
    'currentProviderSupport',
    function () {
      const {
        isEmpty,
        migrationInProgress,
        currentProviderSupport
      } = this.getProperties(
        'isEmpty',
        'migrationInProgress',
        'currentProviderSupport'
      );
      return currentProviderSupport && !isEmpty && !migrationInProgress;
    }
  ),

  /**
   * Css classes for 'migrate' button
   * @type {Ember.ComputedProperty<string>}
   */
  migrateButtonClasses: computed(
    'pendingActionAnimation',
    'migrationInProgress',
    'migrationEnabled',
    function () {
      let classes = 'action-icon toolbar-icon ';
      const {
        pendingActionAnimation,
        migrationInProgress,
        migrationEnabled,
      } = this.getProperties(
        'pendingActionAnimation',
        'migrationInProgress',
        'migrationEnabled'
      );
      if (migrationInProgress) {
        classes += 'disabled ' + pendingActionAnimation;
      } else if (!migrationEnabled) {
        classes += 'disabled';
      }
      return classes;
    }
  ),

  /**
   * Css classes for 'replicate' button
   * @type {Ember.ComputedProperty<string>}
   */
  replicateButtonClasses: computed(
    'pendingActionAnimation',
    'replicationInProgress',
    'replicationEnabled',
    function () {
      let classes = 'action-icon toolbar-icon ';
      const {
        pendingActionAnimation,
        replicationInProgress,
        replicationEnabled,
      } = this.getProperties(
        'pendingActionAnimation',
        'replicationInProgress',
        'replicationEnabled'
      );
      if (replicationInProgress) {
        classes += 'disabled ' + pendingActionAnimation;
      } else if (!replicationEnabled) {
        classes += 'disabled';
      }
      return classes;
    }
  ),

  /**
   * Tooltip text for 'migrate' button
   * @type {Ember.ComputedProperty<string>}
   */
  migrateButtonTooltip: computed('migrationInProgress', 'migrationEnabled', function () {
    // const {
    //   migrationInProgress,
    //   migrationEnabled,
    // } = this.getProperties(
    //   'migrationInProgress',
    //   'migrationEnabled'
    // );
    return 'todo - describe me';
  }),

  /**
   * Tooltip text for 'replicate' button
   * @type {Ember.ComputedProperty<string>}
   */
  replicateButtonTooltip: computed('replicationInProgress', 'replicationEnabled', function () {
    // const {
    //   replicationInProgress,
    //   replicationEnabled,
    // } = this.getProperties(
    //   'replicationInProgress',
    //   'replicationEnabled'
    // );
    return 'todo - describe me';
  }),

  /**
   * @type {File}
   */
  file: computed.reads('fileDistribution.file'),
  
  /**
   * @type {string}
   */
  providerId: computed.reads('fileDistribution.provider'),
  
  // TODO: wait for provider property to load
  /**
   * (args)
   */
  provider: computed.reads('fileDistribution.getProvider'),
  providerName: computed.reads('provider.name'),
  
  isEmpty: computed.reads('fileDistribution.isEmpty'),
  isComplete: computed.reads('fileDistribution.isComplete'),
  neverSynchronized: computed.reads('fileDistribution.neverSynchronized'),
  
  /**
   * Collection of transfers for current file and provider
   * @type {Ember.Array<Transfer>}
   */
  fileProviderTransfers: computed('fileTransfers.[]', function () {
    const fileTransfers = this.get('fileTransfers');
    const providerId = this.get('providerId');
    if (fileTransfers) {
      return fileTransfers.filter(t => get(t, 'provider') === providerId);
    }
  }),
  
  /**
   * - If replication is in progress for this file: 'replication'
   * - If migration is in progress for this file: 'migration'
   * - If none of the above: null
   * - If transfers data is not loaded yet: undefined
   * @type {string|null|undefined}
   */
  replicationType: computed('fileProviderTransfers.[]', function () {
    const fileProviderTransfers = this.get('fileProviderTransfers');
    if (fileProviderTransfers) {
      if (!isEmpty(fileProviderTransfers)) {
        if (fileProviderTransfers.some(t => get(t, 'isOngoing'))) {
          return 'migration';
        } else {
          return 'replication';
        }
      } else {
        return null;
      }
    }
  }),
  
  actions: {
    /**
     * Opens migration popover that allows to choose to what provider migrate
     */
    openMigrationOptions() {
      this.openMigrationOptions(this.get('provider'));
    },
    // TODO: this cannot be used until provider property is loaded
    startReplication() {
      this.startReplication(this.get('providerId'));
    },
  },
});
