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
