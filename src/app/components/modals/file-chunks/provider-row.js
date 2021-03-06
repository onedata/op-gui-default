/**
 * A row in data distribution table that represents distribution of file data
 * on single provider.
 *
 * @module components/modals/file-chunks/provider-row
 * @author Jakub Liput, Michal Borzecki
 * @copyright (C) 2017-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import _ from 'lodash';

const {
  Component,
  computed,
  get,
  isEmpty,
  inject: { service },
  String: { capitalize },
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
   * One of: "single-provider", "proxy-provider", null
   * @type {string|null}
   */
  transferDisabledReason: null,

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
   * @returns {Promise<Transfer>}
   */
  startEviction: () => {},

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
   * True if migration has been started by user but request is not completed yet
   * @type {boolean}
   */
  migrationInvoked: false,
  
  /**
   * If true, do not allow to invoke other actions for provider when there
   * are pending transfers for it
   * @type {boolean}
   */
  disableActionInProgress: false,
  
  /**
   * @type {boolean}
   */
  transferEnabled: computed('transferDisabledReason', function () {
    return isEmpty(this.get('transferDisabledReason'));
  }),

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
   * True if eviction has been started by user but request is not completed yet
   * @type {boolean}
   */
  evictionInvoked: false,

  /**
   * True if transfers options should be inactive (not-clickable).
   * Currently always false, as we do not want lock other operations when
   * there are pending transfers.
   * @type {boolean}
   */
  transferLocked: false,

  replicationInProgress: computed('transferTypes', 'replicationInvoked', function () {
    return _.includes(this.get('transferTypes'), 'replication-destination') ||
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

  migrationInProgress: computed('transferTypes', 'migrationInvoked', function () {
    return _.includes(this.get('transferTypes'), 'migration-source') ||
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
   * @type {Ember.ComputedProperty<boolean>}
   */
  evictionInProgress: computed(
    'transferTypes',
    'evictionInvoked',
    function () {
      return _.includes(this.get('transferTypes'), 'eviction') ||
        this.get('evictionInvoked');
    }
  ),

  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  evictionEnabled: computed(
    'neverSynchronized',
    'isEmpty',
    'transferEnabled',
    'file.isDir',
    'hasBlocksToEvict',
    function () {
      const {
        neverSynchronized,
        isEmpty,
        transferEnabled,
        file,
        hasBlocksToEvict,
      } = this.getProperties(
        'neverSynchronized',
        'isEmpty',
        'transferEnabled',
        'file',
        'hasBlocksToEvict'
      );

      return transferEnabled &&
        (get(file, 'isDir') ?
          true :
          (!neverSynchronized && !isEmpty && hasBlocksToEvict)
        );
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
    'disableActionInProgress',
    function () {
      let classes = 'action-icon toolbar-icon ';
      const {
        pendingActionAnimation,
        migrationInProgress,
        migrationEnabled,
        disableActionInProgress,
      } = this.getProperties(
        'pendingActionAnimation',
        'migrationInProgress',
        'migrationEnabled',
        'disableActionInProgress'
      );
      if (migrationInProgress) {
        classes += disableActionInProgress ? 'disabled ' : '' + pendingActionAnimation;
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
    'disableActionInProgress',
    function () {
      let classes = 'action-icon toolbar-icon ';
      const {
        pendingActionAnimation,
        replicationInProgress,
        replicationEnabled,
        disableActionInProgress,
      } = this.getProperties(
        'pendingActionAnimation',
        'replicationInProgress',
        'replicationEnabled',
        'disableActionInProgress'
      );
      if (replicationInProgress) {
        classes += disableActionInProgress ? 'disabled ' : '' + pendingActionAnimation;
      } else if (!replicationEnabled) {
        classes += 'disabled';
      }
      return classes;
    }
  ),

  /**
   * Css classes for 'evict' button
   * @type {Ember.ComputedProperty<string>}
   */
  evictButtonClasses: computed(
    'pendingActionAnimation',
    'evictionInProgress',
    'evictionEnabled',
    'disableActionInProgress',
    function () {
      let classes = 'action-icon toolbar-icon ';
      const {
        pendingActionAnimation,
        evictionInProgress,
        evictionEnabled,
        disableActionInProgress,
      } = this.getProperties(
        'pendingActionAnimation',
        'evictionInProgress',
        'evictionEnabled',
        'disableActionInProgress'
      );
      if (evictionInProgress) {
        classes += disableActionInProgress ? 'disabled ' : '' + pendingActionAnimation;
      } else if (!evictionEnabled) {
        classes += 'disabled';
      }
      return classes;
    }
  ),

  /**
   * Tooltip text for 'migrate' button
   * @type {Ember.ComputedProperty<string|undefined>}
   */
  migrateButtonTooltip: computed('migrationInProgress', 'migrationEnabled', function () {
    const {
      migrationInProgress,
      migrationEnabled,
      transferDisabledReason,
      isEmpty,
    } = this.getProperties(
      'migrationInProgress',
      'migrationEnabled',
      'transferDisabledReason',
      'isEmpty'
    );

    if (migrationInProgress === true) {
      return this._t('disabledMigrationInProgress');
    } else if (migrationEnabled) {
      return this._t('migrationStart');
    } else if (transferDisabledReason === 'single-provider') {
      return `${capitalize(this._t('migration').toString())} ${this._t('disabledSingleProvider')}`;
    } else if (transferDisabledReason === 'proxy-provider') {
      return `${this._t('disabledProxyProvider')} ${this._t('migration')}`;
    } else if (isEmpty === true) {
      return this._t('disabledMigrationIsEmpty');
    } else {
      return this._t('disabledMigrationUnknown');
    }
  }),

  /**
   * Tooltip text for 'replicate' button
   * @type {Ember.ComputedProperty<string>}
   */
  replicateButtonTooltip: computed('replicationInProgress', 'replicationEnabled',
    function () {
      const {
        replicationInProgress,
        replicationEnabled,
        transferDisabledReason,
        isComplete,
      } = this.getProperties(
        'replicationInProgress',
        'replicationEnabled',
        'transferDisabledReason',
        'isComplete'
      );

      if (replicationInProgress === true) {
        return this._t('disabledReplicationInProgress');
      } else if (replicationEnabled) {
        return this._t('replicationStart');
      } else if (transferDisabledReason === 'single-provider') {
        return `${capitalize(this._t('replication').toString())} ${this._t('disabledSingleProvider')}`;
      } else if (transferDisabledReason === 'proxy-provider') {
        return `${this._t('disabledProxyProvider')} ${this._t('replication')}`;
      } else if (isComplete === true) {
        return this._t('disabledReplicationIsComplete');
      } else {
        return this._t('disabledReplicationUnknown');
      }
    }
  ),
  
  /**
   * Tooltip text for 'evict' button
   * @type {Ember.ComputedProperty<string>}
   */
  evictButtonTooltip: computed('evictionInProgress', 'evictionEnabled',
    function () {
      const {
        evictionInProgress,
        evictionEnabled,
        transferDisabledReason,
        hasBlocksToEvict,
      } = this.getProperties(
        'evictionInProgress',
        'evictionEnabled',
        'transferDisabledReason',
        'hasBlocksToEvict'
      );

      if (evictionInProgress === true) {
        return this._t('disabledEvictionInProgress');
      } else if (evictionEnabled) {
        return this._t('evictionStart');
      } else if (transferDisabledReason === 'single-provider') {
        return capitalize(this._t('eviction').toString()) + ' ' +
          this._t('disabledSingleProvider');
      } else if (transferDisabledReason === 'proxy-provider') {
        return `${this._t('disabledProxyProvider')} ${this._t('eviction')}`;
      } else if (hasBlocksToEvict === false) {
        return this._t('disabledEvictionNoBlocks');
      } else {
        return this._t('disabledEvictionUnknown');
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

  /**
   * The file has no block on this provider (either is empty or never synchronized)
   */
  isEmpty: computed('fileDistribution.{isEmpty,neverSynchronized}', function () {
    return this.get('fileDistribution.neverSynchronized') || this.get('fileDistribution.isEmpty');
  }),
  
  isComplete: computed.reads('fileDistribution.isComplete'),
  neverSynchronized: computed.reads('fileDistribution.neverSynchronized'),

  /**
   * Collection of transfers for current file and provider
   * @type {Ember.Array<Transfer>}
   */
  fileProviderTransfers: computed(
    'fileTransfers.@each.{replicatingProvider,evictingProvider}',
    function () {
      const fileTransfers = this.get('fileTransfers');
      const providerId = this.get('providerId');
      if (fileTransfers) {
        return fileTransfers.filter(t =>
          get(t, 'replicatingProvider') === providerId ||
          get(t, 'evictingProvider') === providerId
        );
      }
    }
  ),

  transfersCount: computed.reads('fileProviderTransfers.length'),

  /**
   * Possible elements in array:
   * - If it's eviction: 'eviction'
   * - If it's migration source: 'migration-source'
   * - If it's only a replication destination: 'replication-destination'
   * - If there is no transfers: null
   * @type {Array<string>|null}
   */
  transferTypes: computed(
    'fileProviderTransfers.@each.{evictingProvider,replicatingProvider}',
    'providerId',
    function getTransferType() {
      const fileProviderTransfers = this.get('fileProviderTransfers');
      const providerId = this.get('providerId');
      if (fileProviderTransfers && !isEmpty(fileProviderTransfers)) {
        const types = [];
        for (let t of fileProviderTransfers) {
          if (get(t, 'evictingProvider') === providerId) {
            if (get(t, 'replicatingProvider')) {
              types.push('migration-source');              
            } else {
              types.push('eviction');
            }
          } else if (get(t, 'replicatingProvider') === providerId) {
            types.push('replication-destination');
          }
        }
        return types;
      } else {
        return null;
      }
    }),

  /**
   * Translate for this component
   * @param {string} key i18n key for this component
   * @returns {string} translated message
   */
  _t(key) {
    return this.get('i18n').t(I18N_PREFIX + key);
  },

  actions: {
    /**
     * Opens migration popover that allows to choose to what provider migrate
     */
    openMigrationOptions() {
      const {
        migrationEnabled,
        provider,
        transferTypes,
      } = this.getProperties('migrationEnabled', 'provider', 'transferTypes');
      if (migrationEnabled) {
        this.get('openMigrationOptions')(provider, {
          transfersPending: !_.isEmpty(transferTypes),
        });
      }
    },
    // TODO: this cannot be used until provider property is loaded
    startReplication() {
      const {
        replicationEnabled,
        providerId,
        transferTypes,
      } = this.getProperties('replicationEnabled', 'providerId', 'transferTypes');
      if (replicationEnabled) {
        this.set('replicationInvoked', true);
        return this.get('startReplication')(providerId, {
          transfersPending: !_.isEmpty(transferTypes),
        })
          .finally(() => this.set('replicationInvoked', false));
      }
    },
    startEviction() {
      const {
        evictionEnabled,
        providerId,
        transferTypes,
      } = this.getProperties('evictionEnabled', 'providerId', 'transferTypes');
      if (evictionEnabled) {
        return this.get('startEviction')(providerId, {
          transfersPending: !_.isEmpty(transferTypes),
        });
      }
    }
  },
});
