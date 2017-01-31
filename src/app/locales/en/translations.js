import sessionLocales from 'ember-cli-onedata-common/locales/en/session';
import filePermissions from './file-permissions';

/**
 * English translation of GUI strings.
 * Translations dictionary is organized as in routes dir.
 *
 * @module locales/en/translations
 * @author Jakub Liput
 * @copyright (C) 2016-2017 ACK CYFRONET AGH
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
      manageProviders: 'Manage account',
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
      providers: 'providers'
    },
    modals: {
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
    spacesMenu: {
      title: 'spaces',
      create: 'Create',
      join: 'Join',
      drop: {
        setHome: 'Set as home',
        moveUp: 'Move up',
        moveDown: 'Move down',
        leave: 'Leave space',
        rename: 'Rename',
        remove: 'Remove',
        inviteGroup: 'Invite group',
        inviteUser: 'Invite user',
        getSupport: 'Get support'
      },
      createModal: {
        title: 'Create a new space',
        enterName: 'Enter new space name:'
      },
      joinModal: {
        title: 'Join a space',
        label: 'Enter a token of a space to join:'
      },
      renameModal: {
        title: 'Rename a space',
        label: 'Enter new space name:'
      },
      leaveModal: {
        title: 'Leave a space',
        label: 'Are you sure you want to leave space "{{spaceName}}"?'
      },
      removeModal: {
        title: 'Remove a space',
        label: 'Are you sure you want to remove the "{{spaceName}}" space?'
      },
      notify: {
        setAsHomeSuccess: 'Space "{{spaceName}}" set as home',
        setAsHomeFailed: 'Space "{{spaceName}}" cannot be set as home due to an error',
        createSuccess: 'Space "{{spaceName}}" created successfully',
        createFailed: 'Space "{{spaceName}}" cannot be created due to an error',
        leaveSuccess: 'Space "{{spaceName}}" left successfully',
        leaveFailed: 'Cannot leave space "{{spaceName}}" due to an error',
        removeSuccess: 'Space "{{spaceName}}" has been removed',
        removeFailed: 'Failed to remove space "{{spaceName}}"',
        joinSuccess: 'Successfully joined space "{{spaceName}}"',
        joinFailed: 'Cannot join space: "{{errorDetails}}"'
      }
    },
    spacesSubmenu: {
      users: 'users',
      groups: 'groups',
      providers: 'providers'
    },
    groupsSubmenu: {
      members: 'members',
      spaces: 'spaces'
    },
    groupsMenu: {
      title: 'groups',
      create: 'Create',
      join: 'Join',
      drop: {
        moveUp: 'Move up',
        moveDown: 'Move down',
        leave: 'Leave this group',
        leaveParentGroup: 'Leave parent group...',
        rename: 'Rename',
        remove: 'Remove',
        inviteUser: 'Invite user',
        inviteGroup: 'Invite group',
        joinSpace: 'Join space',
        joinAsSubgroup: 'Join as subgroup'
      },
      createModal: {
        title: 'Create a new group',
        enterName: 'Enter new group name:'
      },
      joinModal: {
        title: 'Join a group',
        label: 'Enter a token of a group to join:'
      },
      joinAsSubgroupModal: {
        title: 'Join a group to group',
        label: 'Enter a token of a group to make "{{groupName}}" its subgroup:'
      },
      joinSpaceModal: {
        title: 'Join a space',
        label: 'Enter a token of a space to join the group "{{groupName}}" to it:'
      },
      renameModal: {
        title: 'Rename a group',
        label: 'Enter new group name:'
      },
      leaveModal: {
        title: 'Leave the group',
        label: 'Are you sure you want to leave group "{{groupName}}"?'
      },
      leaveParentGroupModal: {
        title: 'Leave a parent group...',
        label: 'Choose a group, that "{{subgroupName}}" should leave:'
      },
      removeModal: {
        title: 'Remove a group',
        label: 'Are you sure you want to remove the "{{groupName}}" group?'
      },
      notify: {
        createSuccess: 'Group "{{name}}" created successfully',
        createFailed: 'Group "{{name}}" cannot be created due to an error',
        leaveSuccess: 'Group "{{name}}" left successfully',
        leaveFailed: 'Cannot leave group "{{name}}" due to an error',
        leaveParentGroupSuccess: 'Group "{{parentGroupName}}" left successfully by "{{subgroupName}}"',
        leaveParentGroupFailed: 'Group "{{subgroupName}}" cannot leave parent group "{{parentGroupName}}" due to an error',
        removeSuccess: 'Group "{{name}" has been removed',
        removeFailed: 'Failed to remove group "{{name}}"',
        joinSpaceSuccess: 'Successfully joined group "{{groupName}}" to space "{{spaceName}}"',
        joinSpaceFailed: 'Failed to join "{{groupName}}" to some space',
        joinAsSubgroupSuccess: 'Successfully joined group "{{thisGroupName}}" to group "{{groupName}}"',
        joinAsSubgroupFailed: 'Failed to join "{{groupName}}" to some group as a subgroup',
        joinSuccess: 'Successfully joined to group "{{groupName}}"',
        joinFailed: 'Failed to join group'

      }
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
    permissionsTable: {
      save: 'save',
      discard: 'discard',
      inviteButton: {
        user: 'Invite user',
        group: 'Invite group'
      },
      viewSpace: 'view space',
      modifySpace: 'modify space',
      setPrivileges: 'set privileges',
      removeSpace: 'remove space',
      inviteUser: 'invite user',
      removeUser: 'remove user',
      inviteGroup: 'invite group',
      removeGroup: 'remove group',
      inviteProvider: 'invite provider',
      removeProvider: 'remove provider',
      viewGroup: 'view group',
      modifyGroup: 'modify group',
      createSpace: 'create space',
      joinSpace: 'join space',
      leaveSpace: 'leave space',
      getSupport: 'get support',
      removeSubgroup: 'remove subgroup',
      joinGroup: 'join group',
      manageShares: 'manage shares',
      writeFiles: 'write files',
      inviteModal: {
        title: 'Invite {{type}} to space',
        label: 'Pass the below token to the {{type}} you want to invite'
      },
      notify: {
        saveFailedAny: 'Some of permissions saving failed',
        saveFailedSingle: 'Cannot set permissions for "{{name}}"'
      },
      tableTitle: {
        users: 'Users',
        groups: 'Groups'
      },
      noUsers: {
        space: 'This space has no users',
        group: 'This group has no users',
      },
      noGroups: {
        group: 'This group has no subgroups',
        space: 'This space has no groups'
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
      uploadIndicator: 'Uploading: {{currentlyUploadingCount}} file(s) left...',
      uploadFinishedWait: 'Finalizing the upload...'
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
        chunks: 'Show file distribution',
        metadata: 'Edit metadata',
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
        title: 'File distribution',
        text: 'Distribution of file blocks among providers for file',
        providerName: 'Provider',
        dataDitribution: 'File blocks',
        loading: 'Loading file chunks table...',
        error: 'File chunks table cannot be loaded due to an error'
      },
      notify: {
        createFileFailed: 'File or directory "{{fileName}}" creation failed'
      }
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
    tokenModals: {
      userJoinSpace: {
        title: 'Invite user to the space',
        label: 'Pass the below token to the user you want to invite to space'
      },
      groupJoinSpace: {
        title: 'Invite group to the space',
        label: 'Pass the below token to the group you want to invite to space'
      },
      providerSupport: {
        title: 'Get support for the space',
        label: 'Pass the below token to the provider you want to request support from'
      },
      userJoinGroup: {
        title: 'Invite user to the group',
        label: 'Pass the below token to the user you want to invite to group'
      },
      groupJoinGroup: {
        title: 'Invite group to the group',
        label: 'Pass the below token to the group you want to invite to group'
      },
    },
    shareInfoHead: {
      path: 'Path',
      publicUrl: 'Public URL',
      publicHandle: 'Public handle',
      publish: 'Publish',
    }
  },
  notFound: {
    notifyMessage: 'Requested path not found'
  },
  login: {
    message: "Authenticating..."
  },
  groups: {
    title: 'Groups',
    show: {
      title: 'Group settings',
      members: {
        title: 'Members permissions'
      },
      spaces: "Spaces",
      viewPrivileges: "view members of this group"
    }
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
  spaces: {
    title: 'Spaces',
    show: {
      title: 'Space settings',
      users: {
        title: 'Users permissions'
      },
      groups: {
        title: 'Groups permissions'
      },
      viewMembersPrivileges: 'view members of this space'
    }
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
  }
};
