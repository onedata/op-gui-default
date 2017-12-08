/**
 * A row in data distribution table that represents distribution of file data
 * on single provider.
 *
 * @module components/modals/file-chunks/provider-row
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';

const {
  Component,
  computed,
  get,
  isEmpty,
  inject: { service },
} = Ember;

const I18N_PREFIX = 'components.dataFilesListToolbar.fileChunksModal.providerRow.';

export default Component.extend({
  i18n: service(),
  
  tagName: 'tr',
  classNames: ['provider-row'],
  
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
  transferEnabled: true,
  
  /**
   * @virtual
   * @type {Array<Transfer>|undefined}
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
   * @returns {Promise<Transfer>}
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

  /**
   * True if migration has been started by user but request is not completed yet
   * @virtual
   * @type {boolean}
   */
  migrationInvoked: false,
    
  //#endregion
  
  /**
   * Css classes for pending action
   * @type {string}
   */
  pendingActionAnimation: 'in-progress animated infinite semi-hinge pulse-mint',

  /**
   * True if replication has been started by user but request is not completed yet
   * @type {boolean}
   */
  replicationInvoked: false,

  /**
   * True if transfers options should be inactive (not-clickable)
   * @type {Ember.ComputedProperty<boolean>}
   */
  transferLocked: computed.or('migrationInProgress', 'replicationInProgress'),
    
  replicationInProgress: computed('transferType', 'replicationInvoked', function () {
    return this.get('transferType') === 'replication-destination' ||
      this.get('replicationInvoked');
  }),
  
  replicationEnabled: computed(
    'neverSynchronized',
    'isComplete',
    'transferLocked',
    'transferEnabled',
    'file.isDir',
    function () {
      const {
        neverSynchronized,
        isComplete,
        transferLocked,
        transferEnabled,
        file,
      } = this.getProperties(
        'neverSynchronized',
        'isComplete',
        'transferLocked',
        'transferEnabled',
        'file'
      );
      return transferEnabled &&
        (get(file, 'isDir') ? true : (neverSynchronized || !isComplete)) &&
        !transferLocked;
    }
  ),
  
  migrationInProgress: computed('transferType', 'migrationInvoked', function () {
    return this.get('transferType') === 'migration-source' ||
      this.get('migrationInvoked');
  }),
  
  migrationEnabled: computed(
    'neverSynchronized',
    'isEmpty',
    'transferEnabled',
    'transferLocked',
    'file.isDir',
    function () {
      const {
        neverSynchronized,
        isEmpty,
        transferLocked,
        transferEnabled,
        file,
      } = this.getProperties(
        'neverSynchronized',
        'isEmpty',
        'transferLocked',
        'transferEnabled',
        'file'
      );      
      
      return transferEnabled &&
        (get(file, 'isDir') ? true : (!neverSynchronized && !isEmpty)) &&
        !transferLocked;
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
    const {
      i18n,
      migrationInProgress,
      migrationEnabled,
    } = this.getProperties(
      'i18n',
      'migrationInProgress',
      'migrationEnabled'
    );
    
    if (migrationInProgress) {
      return i18n.t(`${I18N_PREFIX}migrationInProgress`);
    } else if (migrationEnabled) {
      return i18n.t(`${I18N_PREFIX}migrationStart`);
    } else {
      return i18n.t(`${I18N_PREFIX}migrationDisabled`);
    }
  }),

  /**
   * Tooltip text for 'replicate' button
   * @type {Ember.ComputedProperty<string>}
   */
  replicateButtonTooltip: computed('replicationInProgress', 'replicationEnabled', function () {
    const {
      i18n,
      replicationInProgress,
      replicationEnabled,
    } = this.getProperties(
      'i18n',
      'replicationInProgress',
      'replicationEnabled'
    );
    
    if (replicationInProgress) {
      return i18n.t(`${I18N_PREFIX}replicationInProgress`);
    } else if (replicationEnabled) {
      return i18n.t(`${I18N_PREFIX}replicationStart`);
    } else {
      return i18n.t(`${I18N_PREFIX}replicationDisabled`);
    }
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
   * - If it's migration source: 'migration-source'
   * - If it's only a replication destination: 'replication-destination'
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
        this.set('replicationInvoked', true);
        return this.startReplication(providerId)
          .finally(() => this.set('replicationInvoked', false));
      }
    },
  },
});
