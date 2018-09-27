import sessionLocales from 'ember-cli-onedata-common/locales/en/session';
import resourceLoadError from 'ember-cli-onedata-common/locales/en/components/resource-load-error';
import errorInline from 'ember-cli-onedata-common/locales/en/components/error-inline';
import filePermissions from './file-permissions';

/**
 * English translation of GUI strings.
 * Translations dictionary is organized as in routes dir.
 *
 * @module locales/en/translations
 * @author Jakub Liput
 * @copyright (C) 2016-2018 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default {
  common: {
    file: 'file',
    directory: 'directory',
    unknown: 'unknown',
    unknownError: 'Unknown error',
    modal: {
      ok: 'OK',
      cancel: 'Cancel',
      yes: 'Yes',
      no: 'No',
      close: 'Close',
      create: 'Create',
      join: 'Join',
      leave: 'Leave',
      fetchingToken: 'Fetching token...',
      fetchingTokenError: 'Fetching token failed!',
    },
    notify: {
      clipboardSuccess: 'Text has been copied to clipboard.',
      clipboardFailue: 'Text cannot be copied to clipboard - please copy it manually'
    },
    featureNotSupportedShort: 'Feature not supported',
    featureNotSupportedLong: 'Sorry, this feature is not supported yet.',
    cannotLoadResource: 'Cannot load requested resource',
    serverError: 'Server error',
    fatalApplicationErrorResources: "A fatal error occured loading application's resources",
    noPrivileges: 'You do not have privileges to {{privileges}}.'
  },
  services: {
    session: sessionLocales
  },
  components: {
    resourceLoadError,
    errorInline,
    filePermissions: {
      error: 'An error occured when loading permissions data:',
      posix: {
        differentPermissionsMessage:
          'Selected files have different POSIX permissions - you can reset them to common value',
        resetPermissions: 'Set new permissions for all files'
      },
      acl: {
        errorPrefix: 'An error occured on loading ACL settings:',
        errorCannotLoadACL: 'File ACL could not be loaded from server',
        aceItem: {
          selectUser: 'Select a user',
          selectGroup: 'Select a group',
          permissions: filePermissions,
          types: {
            allow: 'Allow',
            deny: 'Deny'
          }
        }
      }
    },
    commonLoader: {
      defaultMessage: 'Loading...',
      synchronizingSpaces: 'Synchronizing user spaces...',
      synchronizingGroups: 'Synchronizing groups...',
      synchronizingShares: 'Synchronizing shares...',
      firstLogin: 'This might take a while if this is the first login to this provider'
    },
    topBar: {
      logout: 'Log out',
      manageAccount: 'Manage account',
      about: 'About this provider',
    },
    mainMenu: {
      data: 'data',
      links: 'links',
      recent: 'recent',
      shared: 'shared',
      trash: 'trash',
      spaces: 'spaces',
      groups: 'groups',
      token: 'tokens',
      providers: 'providers',
      transfers: 'transfers',
    },
    modals: {
      aboutModal: {
        title: 'About this provider',
        providerName: 'Provider name:',
        version: 'Software version:',
        registeredIn: 'Registered in Zone:',
      },
      renameModal: {
        renameSuccess: 'Element "{{oldName}}" renamed to "{{newName}}"',
        renameFailed: 'Element "{{oldName}}" rename to "{{newName}}" failed due to an error',
      },
      removeModal: {
        removeSuccess: 'Element "{{name}}" has been removed',
        removeFailed: 'Element "{{name}}" cannot be removed due to an error',
      },
      filePermissions: {
        title: 'Edit permissions',
        permissionsType: 'Permissions type',
        mixedPermissionsMessage: 'Selected files have mixed permission types (POSIX/ACL). ' +
          'Please select new permissions type in top of this modal if tou want to set all permissions to common value.',
        eaccessMessage: 'You do not have access to view permissions of selected files. However, you can choose POSIX or ACL permissions to try to reset them.',
        submitSuccess: 'New permissions for selected files have been set',
        submitFailed: 'Setting new permissions for selected files failed!',
        submitFailedSome: 'Setting new permissions for {{failedCount}} of {{filesCount}} selected files failed',
        errorMessage: 'An unexpected error occured',
        types: {
          posix: 'POSIX',
          acl: 'ACL'
        }
      },
      createShare: {
        title: 'Share the directory',
        submitSuccess: 'Share created sucessfully',
        submitFailed: 'Share creation failed',
        rowName: {
          label: 'Name',
          sublabel: 'Set a name for created share that will be visible to other users'
        },
      },
      publishShare: {
        title: "Publish the share",
        selectHandleServicePlaceholder: "Choose a handle service",
        publishButton: "Publish",
        noHandleServices: 'Unfortunately, currently you do not have access to any handle service.',
        dublinCoreLabel: 'Please enter Dublin Core metadata here:',
        publishFailure: 'Publishing share "{{shareName}}" failed: {{errorMessage}}',
        publishSuccess: 'Share "{{shareName}}" has been published successfully!',
      },
      fileShareInfo: {
        title: 'Share summary',
        subtitle: 'A directory with path "{{path}}" is shared with name "{{name}}"',
        goToShare: 'Open the share',
        publicUrl: {
          label: 'Public URL',
          sublabel: 'You can give an acces to the shared directory to anyone with the public URL:'
        }
      }
    },
    transfersMenu: {
      title: 'space data transfers',
    },
    sharesMenu: {
      title: 'shares',
      join: 'Join',
      drop: {
        rename: 'Rename',
        remove: 'Remove'
      },
      renameModal: {
        title: 'Rename share',
        label: 'Enter new share name:'
      },
      removeModal: {
        title: 'Remove share',
        label: 'Are you sure that you want to remove the "{{name}}" share?'
      }
    },
    // data
    dataFilesTree: {
      rootDirectory: 'Root directory'
    },
    dataFilesList: {
      files: 'files',
      size: 'size',
      modification: 'modification',
      permissions: 'permissions',
      fileIsBroken: 'This file is not synced yet',
      updatingMessage: 'Updating files list...',
      cannotFetchMoreFiles: 'Tried fetch to more files from server, but it failed: {{errorMessage}}',
      downloadError: {
        title: 'Cannot download file',
        message: 'File "{{fileName}}" cannot be downloaded because of server error: {{errorMessage}}'
      },
      fileMetadataRow: {
        metadataDeleteFailed: 'Could not delete metadata for file "{{fileName}": {{errorMessage}}',
        metadataDeleteSuccess: 'Deleted metadata for file "{{fileName}}"',
      },
      fileInfoRow: {
        cdmiObjectId: 'File ID',
        spaceId: 'Space ID',
        name: {
          file: 'File name',
          dir: 'Directory name',
        },
        path: {
          file: 'File path',
          dir: 'Directory path',
        },
      },
      uploadIndicator: 'Uploading: {{currentlyUploadingCount}} file(s) left...',
      uploadFinishedWait: 'Finalizing the upload...',
      noViewPrivileges: 'view contents of this directory',
    },
    metadataPanel: {
      saveAllChanges: 'Save all changes',
      discardChanges: 'Discard changes',
      removeMetadata: 'Remove metadata',
      saveSuccess: 'Metadata saved successfully',
      saveFailure: 'Cannot save metadata: {{errorMessage}}',
    },
    dataFilesListToolbar: {
      tooltip: {
        createDir: 'Create directory',
        createFile: 'Create file',
        shareFile: 'Share element',
        uploadFile: 'Upload file',
        renameFile: 'Rename element',
        permissions: 'Change element permissions',
        copy: 'Copy element',
        cut: 'Cut element',
        remove: 'Remove element',
        chunks: 'Show data distribution',
        metadata: 'Edit metadata',
        info: 'Toggle element info',
      },
      renameFileModal: {
        title: 'Rename file or directory',
        enterName: 'Rename the item "{{currentName}}" to:',
        success: '{{type}} "{{oldName}}" has been renamed to "{{newName}}"',
        failure: '{{type}} "{{oldName}}" could not be renamed due to an error: {{errorMessage}}'
      },
      createDirModal: {
        title: 'New directory',
        enterName: 'Enter new directory name:'
      },
      createFileModal: {
        title: 'New file',
        enterName: 'Enter new file name:'
      },
      removeFilesModal: {
        title: 'Remove files',
        text: 'Do you want to remove {{selectedCount}} selected element(s)?',
        notify: {
          singleRemoveSuccess: '"{{fileName}}" has been removed',
          multipleRemoveSuccess: '{{filesCount}} elements have been removed',
          singleRemoveFailed: '"{{fileName}}" could not be removed: {{errorMessage}}',
          multipleRemoveFailed: 'Failed to remove {{failCount}} out of {{filesCount}} elements',
          allRemoveFailed: 'Failed to remove all selected elements',
        }
      },
      // TODO: to remove
      editPermissionsModal: {
        text: 'Enter new file permissions code:'
      },
      fileChunksModal: {
        file: 'file',
        directory: 'directory',
        rootDirectory: 'root directory',
        title: 'Data distribution',
        text: 'Management of data distribution for',
        fileIsEmpty: 'This file has no content.',
        neverSynchronized: 'Never synchronized',
        neverSynchronizedHint: 'This file was never read or modified on selected ' +
          'provider. File blocks will be synchronized when needed.  ' +
          'You can also manually replicate the file to selected provider',
        providerName: 'Provider',
        dataDistribution: 'Data blocks',
        migrateFileDataInto: 'Migrate the data into',
        loading: 'Loading file distribution data...',
        error: 'Data distribution table cannot be loaded due to an error',
        noCurrentProviderSupport: 'Current space is not supported by this ' +
          'provider, thus advanced data replication or migration features are ' +
          'not available here. To access them, visit one of the supporting ',
        providersLink: 'providers',
        onlySingleProviderSupport: 'Current space is supported by only one provider, ' +
          'thus advanced data replication or migration features are not available.',
        currentlyTransferredText: 'This {{elementType}} is currently transferred between ' +
          'providers',
        currentlyTransferredLink: 'see ongoing transfers',
        endedTransfersText: 'This {{elementType}} was transferred manually',
        orMore: 'or more',
        time: 'time',
        times: 'times',
        endedTransfersLink: 'see history',
        noTransfersText: 'This {{elementType}} has never been transferred manually',
        providerRow: {
          replication: 'replication',
          migration: 'migration',
          invalidation: 'invalidation',
          migrationStart: 'Migrate the data to other provider...',
          replicationStart: 'Replicate the data to selected provider',
          invalidationStart: 'Invalidate redundant data blocks on this provider',
          disabledSingleProvider: 'is available only with two or more supporting providers',
          disabledProxyProvider: 'Visit a supporting provider in order to schedule',
          disabledMigrationIsEmpty: 'Cannot schedule migration as there are no file blocks on this provider',
          disabledMigrationInProgress: 'The data is currently migrated from selected provider',
          disabledReplicationIsComplete: 'Cannot schedule replication as all file block are already on this provider',
          disabledReplicationInProgress: 'The data is currently replicated to selected provider',
          disabledInProgress: 'Operation is unavailable as there are transfers in progress on this provider',
          disabledInvalidationNoBlocks: 'Invalidation is not possible unless some data blocks on this provider are redundant',
          disabledInvalidationInProgress: 'The data is currently invalidated in selected provider',
          disabledMigrationUnknown: 'The data cannot be migrated from selected provider now',
          disabledReplicationUnknown: 'The data cannot be replicated into selected provider now',
          disabledInvalidationUnknown: 'The data cannot be invalidated in selected provider now',
        },
        migratePopover: {
          migrateItem: {
            busy: 'busy',
          },
        },
        confirmStart: 'Start',
        confirmType: {
          replicate: 'replication',
          migrate: 'migration',
          invalidate: 'invalidation',
        },
        pendingTransfersWarning: 'There are pending transfers of the file in this provider. Starting a new {{type}} can interrupt existing transfers. Do you want to start a new {{type}} anyway?',
        confirmDialogTitle: 'Warning',
      },
      notify: {
        createFileFailed: 'File or directory "{{fileName}}" creation failed'
      },
    },
    fileUpload: {
      titleUpload: 'Uploading {{count}} file(s)',
      uploading: 'Uploading',
      cancelAll: 'Cancel all',
    },
    uploadingFile: {
      error: "Error:",
      completed: "Completed!"
    },
    shareInfoHead: {
      path: 'Path',
      publicUrl: 'Public URL',
      publicHandle: 'Public handle',
      publish: 'Publish',
    },
    transfers: {
      providersMapOfDist: 'Ongoing transfers map',
      scheduledTransfers: 'Waiting',
      activeTransfers: 'Ongoing',
      completedTransfers: 'Ended',
      onTheFlyTransfers: 'On-the-fly',
      noTransfers: {
        file: 'There are no transfers for selected file or directory',
        scheduled: 'There are no waiting transfers',
        current: 'There are no ongoing transfers',
        completed: 'There are no ended transfers',
      },
      initializingTransfers: 'Initializing transfers...',
      notSupported: 'Cannot list transfers of selected space because it is not ' +
        'supported by current provider',
      in: 'Input',
      out: 'Output',
      fileHistoryLimitReached: 'History limit per file reached',
      fileNotExists: 'Selected file or directory does not exist',
      liveTableStats: {
        type: 'Type',
        path: 'File/directory',
        userName: 'Username',
        destination: 'Destination',
        scheduledAt: 'Scheduled at',
        startedAt: 'Started at',
        finishedAt: 'Finished at',
        totalBytes: 'Replicated',
        totalFiles: 'Processed files',
        status: 'Status',
        destinationUnknown: '-',
        cancelFailure: 'Error occurred during transfer cancellation.',
        rerunFailure: 'Error occurred during transfer rerun.',
        rerunStarting: 'Rerunning transfer...',
        rerunSuccess: 'Rerun transfer may be found in "Waiting" tab.',
        cellActions: {
          cancelTransfer: 'Cancel transfer',
          rerunTransfer: 'Rerun transfer',
        },
        cellFileName: {
          deleted: 'deleted',
        },
        cellStatus: {
          completed: 'Completed',
          skipped: 'Skipped',
          cancelled: 'Cancelled',
          failed: 'Failed',
          replicating: 'Replicating',
          scheduled: 'Scheduled',
          enqueued: 'Enqueued',
          aborting: 'Aborting',
          invalidating: 'Invalidating',
        },
        cellType: {
          replication: 'Replication',
          migration: 'Migration',
          invalidation: 'Invalidation',
        },
        cellTotalFiles: {
          invalidated: 'invalidated',
          replicated: 'replicated',
        },
      },
      transferChart: {
        minute: 'Minute',
        hour: 'Hour',
        day: 'Day',
        month: 'Month',
        time: 'Time',
        throughput: 'Throughput',
        output: 'Output',
        waitingForTransferStart: 'Waiting for the transfer to start...',
        noStatsForUnit: 'No activity in the last {{timeUnit}}.',
        waitingForStats: 'Gathering transfer statistics...',
        waitingForStatsTip: 'Statistics are delayed due to synchronization ' +
          'latency caused by data distribution.',
      },
      throughputDistribution: {
        title: 'Providers throughput',
        input: 'Input',
        output: 'Output',
        receivedFrom: 'Received from',
        sentTo: 'Sent to',
        timeLastUpdate: 'Time (last update: {{lastUpdate}})',
        throughput: 'Throughput',
        onTheFly: 'On-the-fly',
        all: 'All',
        jobs: 'Jobs',
        providerSelector: {
          allProviders: 'All providers',
        },
      },
      transfersOverview: {
        hide: 'Hide transfers overview',
        show: 'Show transfers overview',
      },
    },
    rootDirSettingsDrop: {
      drop: {
        dataDistribution: 'Data distribution',
      },
    },
  },
  notFound: {
    notifyMessage: 'Requested path not found'
  },
  login: {
    message: "Authenticating..."
  },
  shares: {
    title: 'Shared'
  },
  trash: {
    title: 'Trash'
  },
  recent: {
    title: 'Recent'
  },
  links: {
    title: 'Links'
  },
  data: {
    title: 'Data',
    rootDirectory: 'Root directory',
    dataSpace: {
      title: 'Space ', // should be used as: `{{title}} "{{spaceName}}"`
      nullDir: 'This space is not supported by any providers or cannot be synchronized',
      dir: {
        title: 'File browser',
        file: {
          title: 'File details'
        },
        loaderMessage: 'Loading file browser...',
        error: {
          synchronizeError: 'Selected space or directory cannot be synchronized.'
        }
      }
    }
  },
  transfers: {
    title: 'Transfers',
    show: {
      title: 'Transfers for space'
    },
  },
  spacesError: {
    fetchFailure: 'An error occured on fetching spaces list.',
    noSpaceSupported: 'This provider does not support any space.',
    errorCauses: 'It may be a sync error or failure of the provider.',
    advice: 'Please try to refresh the page or contact system administrator.'
  },
  error: {
    cannotLoadResource: 'A fatal error occured: the requested resource cannot be loaded.',
    tryRefreshOrContact: 'You can try to refresh the page or contact administrators.'
  },
  onezone: {
    cannotResolveUrl: 'Cannot resolve Onezone URL',
  },
};
