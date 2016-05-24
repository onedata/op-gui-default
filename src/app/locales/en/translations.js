/**
 * English translation of GUI strings.
 * Translations dictionary is organized as in routes dir.
 *
 * @module locales/en/translations
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
*/
export default {
  common: {
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
      clipboardSuccess: 'The text copied to clipboard',
      clipboardFailue: 'The text cannot be copied to clipboard - please copy it manually'
    },
    featureNotSupportedShort: 'Feature not supported',
    featureNotSupportedLong: 'Sorry, this feature is not supported yet.',
    cannotLoadResource: 'Cannot load requested resource'
  },
  components: {
    commonLoader: {
      defaultMessage: 'Loading...',
      synchronizingSpaces: 'Synchronizing user spaces...',
      synchronizingGroups: 'Synchronizing groups...',
      firstLogin: 'This might take a while if this is the first login to this provider'
    },
    topBar: {
      logout: 'Log out',
      manageProviders: 'Manage account'
    },
    mainMenu: {
      data: 'data',
      links: 'links',
      recent: 'recent',
      collection: 'collection',
      trash: 'trash',
      spaces: 'spaces',
      groups: 'groups',
      token: 'tokens',
      providers: 'providers'
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
        createSpace: 'Request space creation',
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
        renameSuccess: 'Group "{{oldName}}" renamed to "{{newName}}"',
        renameFailed: 'Group "{{oldName}}" rename to "{{newName}}" failed due to an error',
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
      noGroups: {
        group: 'This group has no subgroups',
        space: 'This space does not belong to any group'
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
      fileIsBroken: 'This file is not synced yet'
    },
    dataFilesListToolbar: {
      tooltip: {
        createDir: 'Create directory',
        createFile: 'Create file',
        shareFile: 'Share element',
        uploadFile: 'Upload file',
        rename: 'Rename element',
        permissions: 'Change element permissions',
        copy: 'Copy element',
        cut: 'Cut element',
        remove: 'Remove element',
        chunks: 'Show file distribution'
      },
      renameFileModal: {
        title: 'Rename file or directory',
        enterName: 'Rename the item "{{currentName}}" to:'
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
        text: 'Do you want to remove selected files?'
      },
      editPermissionsModal: {
        title: 'Edit permissions',
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
      cancelAll: 'Cancel all',
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
      requestSpaceCreation: {
        title: 'Request space creation for the group',
        label: 'Pass the below token to a provider of your choice. The token can be used to create a space for your group and grant support to the space.'
      }
    },
  },
  notFound: {
    notifyMessage: 'Requested path not found'
  },
  groups: {
    title: 'Groups',
    show: {
      title: 'Group settings',
      members: {
        title: 'Members permissions'
      },
      spaces: "Spaces"
    }
  },
  collection: {
    title: 'Collection'
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
        }
      }
    }
  }
};
