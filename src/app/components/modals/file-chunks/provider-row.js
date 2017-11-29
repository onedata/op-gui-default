import Ember from 'ember';

const {
  Component,
  computed,
  get,
  isEmpty,
} = Ember;

export default Component.extend({
  tagName: 'tr',
  
  //#region External properties
  
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
   * @virtual
   * @type {Function}
   */
  startTransfersUpdater: () => {},

  /**
   * @virtual
   * @type {Function}
   */
  stopTransfersUpdater: () => {},

  //#endregion
  
  /**
   * Css classes for pending action
   * @type {string}
   */
  pendingActionAnimation: 'in-progress animated infinite semi-hinge pulse-mint',

  transferLocked: computed.or('migrationInProgress', 'replicationInProgress'),
  
  replicationInProgress: computed.equal('transferType', 'replication-destination'),
  replicationEnabled: computed(
    'neverSynchronized',
    'isComplete',
    'currentProviderSupport',
    'transferLocked',
    function () {
      const {
        neverSynchronized,
        isComplete,
        currentProviderSupport,
        transferLocked,
      } = this.getProperties(
        'neverSynchronized',
        'isComplete',
        'currentProviderSupport',
        'transferLocked'
      );
      return currentProviderSupport && (neverSynchronized || !isComplete) &&
        !transferLocked;
    }
  ),
  
  migrationInProgress: computed.equal('transferType', 'migration-source'),
  migrationEnabled: computed(
    'isEmpty',
    'currentProviderSupport',
    'transferLocked',
    function () {
      const {
        isEmpty,
        currentProviderSupport,
        transferLocked,
      } = this.getProperties(
        'isEmpty',
        'currentProviderSupport',
        'transferLocked'
      );
      return currentProviderSupport && !isEmpty && !transferLocked;
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
    
  // TODO: wait for provider property to load - isLoading computed property
  /**
   * (args)
   */
  provider: computed.reads('fileDistribution.getProvider'),
  providerName: computed.reads('provider.name'),
  
  /**
   * @type {string}
   */
  providerId: computed.reads('fileDistribution.provider'),
  
  isEmpty: computed.reads('fileDistribution.isEmpty'),
  isComplete: computed.reads('fileDistribution.isComplete'),
  neverSynchronized: computed.reads('fileDistribution.neverSynchronized'),
  
  /**
   * Collection of transfers for current file and provider
   * @type {Ember.Array<Transfer>}
   */
  fileProviderTransfers: computed('fileTransfers.@each.{destination,migrationSource}', function () {
    const fileTransfers = this.get('fileTransfers');
    const providerId = this.get('providerId');
    if (fileTransfers) {
      return fileTransfers.filter(t =>
        get(t, 'destination') === providerId || get(t, 'migrationSource') === providerId
      );
    }
  }),
  
  transfersCount: computed.reads('fileProviderTransfers.length'),

  /**
   * - If it's migration source: 'migration'
   * - If it's only a replication destination: 'replication'
   * - If it's none of the above: 'unknown' (should not occur)
   * - If there is no transfers: null
   * @type {string|null}
   */
  transferType: computed(
    'fileProviderTransfers.@each.{migrationSource,destination}',
    'providerId',
    function getTransferType() {
      const fileProviderTransfers = this.get('fileProviderTransfers');
      const providerId = this.get('providerId');
      if (fileProviderTransfers && !isEmpty(fileProviderTransfers)) {
        if (fileProviderTransfers.some(t => get(t, 'migrationSource') === providerId)) {
          return 'migration-source';
        } else if (fileProviderTransfers.some(t => get(t, 'destination') === providerId)) {
          return 'replication-destination';
        } else {
          return 'unknown';
        }
      } else {
        return null;
      }
    }),

  actions: {
    /**
     * Opens migration popover that allows to choose to what provider migrate
     */
    openMigrationOptions() {
      const {
        migrationEnabled,
        provider,
      } = this.getProperties('migrationEnabled', 'provider');
      if (migrationEnabled) {
        this.openMigrationOptions(provider);
      }
    },
    // TODO: this cannot be used until provider property is loaded
    startReplication() {
      const {
        replicationEnabled,
        providerId,
      } = this.getProperties('replicationEnabled', 'providerId');
      if (replicationEnabled) {
        return this.startReplication(providerId);
      }
    },
  },
});
