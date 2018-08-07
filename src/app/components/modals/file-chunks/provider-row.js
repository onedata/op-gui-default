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
  startInvalidation: () => {},

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
   * True if invalidation has been started by user but request is not completed yet
   * @type {boolean}
   */
  invalidationInvoked: false,

  /**
   * True if transfers options should be inactive (not-clickable)
   * @type {Ember.ComputedProperty<boolean>}
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
  invalidationInProgress: computed(
    'transferTypes',
    'invalidationInvoked',
    function () {
      return _.includes(this.get('transferTypes'), 'invalidation') ||
        this.get('invalidationInvoked');
    }
  ),

  /**
   * @type {Ember.ComputedProperty<boolean>}
   */
  invalidationEnabled: computed(
    'neverSynchronized',
    'isEmpty',
    'transferEnabled',
    'transferLocked',
    'file.isDir',
    'hasBlocksToInvalidate',
    function () {
      const {
        neverSynchronized,
        isEmpty,
        transferLocked,
        transferEnabled,
        file,
        hasBlocksToInvalidate,
      } = this.getProperties(
        'neverSynchronized',
        'isEmpty',
        'transferLocked',
        'transferEnabled',
        'file',
        'hasBlocksToInvalidate'
      );

      return transferEnabled &&
        (get(file, 'isDir') ?
          true :
          (!neverSynchronized && !isEmpty && hasBlocksToInvalidate)
        ) && !transferLocked;
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
   * Css classes for 'invalidate' button
   * @type {Ember.ComputedProperty<string>}
   */
  invalidateButtonClasses: computed(
    'pendingActionAnimation',
    'invalidationInProgress',
    'invalidationEnabled',
    'disableActionInProgress',
    function () {
      let classes = 'action-icon toolbar-icon ';
      const {
        pendingActionAnimation,
        invalidationInProgress,
        invalidationEnabled,
        disableActionInProgress,
      } = this.getProperties(
        'pendingActionAnimation',
        'invalidationInProgress',
        'invalidationEnabled',
        'disableActionInProgress'
      );
      if (invalidationInProgress) {
        classes += disableActionInProgress ? 'disabled ' : '' + pendingActionAnimation;
      } else if (!invalidationEnabled) {
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
   * Tooltip text for 'invalidate' button
   * @type {Ember.ComputedProperty<string>}
   */
  invalidateButtonTooltip: computed('invalidationInProgress', 'invalidationEnabled',
    function () {
      const {
        invalidationInProgress,
        invalidationEnabled,
        transferDisabledReason,
        hasBlocksToInvalidate,
      } = this.getProperties(
        'invalidationInProgress',
        'invalidationEnabled',
        'transferDisabledReason',
        'hasBlocksToInvalidate'
      );

      if (invalidationInProgress === true) {
        return this._t('disabledInvalidationInProgress');
      } else if (invalidationEnabled) {
        return this._t('invalidationStart');
      } else if (transferDisabledReason === 'single-provider') {
        return capitalize(this._t('invalidation').toString()) + ' ' +
          this._t('disabledSingleProvider');
      } else if (transferDisabledReason === 'proxy-provider') {
        return `${this._t('disabledProxyProvider')} ${this._t('invalidation')}`;
      } else if (hasBlocksToInvalidate === false) {
        return this._t('disabledInvalidationNoBlocks');
      } else {
        return this._t('disabledInvalidationUnknown');
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
  fileProviderTransfers: computed('fileTransfers.@each.{destination,migrationSource}',
    function () {
      const fileTransfers = this.get('fileTransfers');
      const providerId = this.get('providerId');
      if (fileTransfers) {
        return fileTransfers.filter(t =>
          get(t, 'destination') === providerId || get(t, 'migrationSource') ===
          providerId
        );
      }
    }),

  transfersCount: computed.reads('fileProviderTransfers.length'),

  // FIXME: support multiple types in GUI?
  /**
   * - If it's invalidation: 'invalidation'
   * - If it's migration source: 'migration-source'
   * - If it's only a replication destination: 'replication-destination'
   * - If it's none of the above: 'unknown' (should not occur)
   * - If there is no transfers: null
   * @type {string|null}
   */
  transferTypes: computed(
    'fileProviderTransfers.@each.{migrationSource,destination}',
    'providerId',
    function getTransferType() {
      const fileProviderTransfers = this.get('fileProviderTransfers');
      const providerId = this.get('providerId');
      if (fileProviderTransfers && !isEmpty(fileProviderTransfers)) {
        const types = [];
        for (let t of fileProviderTransfers) {
          if (get(t, 'migration') && get(t, 'migrationSource') === providerId) {
            if (get(t, 'destination')) {
              types.push('migration-source');              
            } else {
              types.push('invalidation');
            }
          } else if (get(t, 'destination') === providerId) {
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
    startInvalidation() {
      const {
        invalidationEnabled,
        providerId,
        transferTypes,
      } = this.getProperties('invalidationEnabled', 'providerId', 'transferTypes');
      if (invalidationEnabled) {
        return this.get('startInvalidation')(providerId, {
          transfersPending: !_.isEmpty(transferTypes),
        });
      }
    }
  },
});
