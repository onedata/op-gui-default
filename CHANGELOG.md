# Release notes for project op-gui-default


CHANGELOG
---------

### Latest changes

* VFS-5380 Fixed redirection to legacy Oneproviders throught Onezone
* VFS-4596 Onedata Unified GUI
* VFS-5038 Added possibility to publish shared directory with no metadata specified
* VFS-5030 Removed debug code from db-index view
* VFS-4980 Added support for index transfers
* VFS-4927 Fixed delay before loading transfers list; error handling
* VFS-3774 Fixed broken request for OpenSans font in Safari if redirecting to Onezone
* VFS-4454 Fix hanging authorization loader spinner
* VFS-4856 Added CDMI object ID and file path info to file row
* VFS-4553 Fixed browser back from Onezone to Oneprovider GUI (wrong redirect and broken WebSocket in Safari)
* VFS-4385 Fixed blank ACL permissions modal after changing permissions
* VFS-4806 Fixed regression bug: file distribution modal for directory has no transfers buttons
* VFS-4791 Support for highly scattered files in file distribution charts
* VFS-4242 Added transfers cancelling and rerunning actions
* VFS-4738 Removed spaces and groups tabs implementation and redirect to Zone GUI spaces and groups
* VFS-4753 Allow to start new transfers if other transfers are pending for file/provider
* VFS-4566 Added transfers list tab for specific file
* VFS-4507 Multiple improvements in transfers list view
* VFS-4538 Better control of invalidation feature availability for files
* VFS-4471 Improved file chunks bar rendering
* VFS-4391 More efficient infinite scroll for transfers list and file transfer status 
* VFS-4487 Fixed not updating completed transfer stats
* VFS-4387 Fixed not updating current transfers stats if user not scrolled view
* VFS-4453 Fixed incorrect total files counter in transfers table
* VFS-3945 Added data invalidation functionality.
* VFS-4355 Added summarized transfer charts per provider.
* VFS-4239 Infinite scroll for transfers lists.
* VFS-4305 Added charts for on-the-fly transfers.
* VFS-4232 Added line chart which shows providers transfer throughput.
* VFS-4260 Added menu for managing space root dir data distribution
* VFS-4157 Requesting completed transfers list with delay to be compatible with backend fixes
* VFS-4223 Fixed long time of loading data distribution modal
* VFS-4027 Added support for peta-, exa-, zetta- and yottabytes
* VFS-4206 Changed speed units on transfers view to Xbits/s
* VFS-4012 Info about remote statistics on transfers view; fixed transfer row sort issues
* VFS-4154 Dynamically adjust polling interval of transfers data; fixed transfer chart loading
* VFS-4088 Fixed incorrect ordering and stacking of transfer chart series
* VFS-4068 Fixed incorrect icons positioning in transfers table
* VFS-4062 Remember opened space when switching between data-spaces-transfers views; fixes in data-space sidebar
* VFS-4059 Fixed provider icon scaling in transfers view
* VFS-4002 Showing transfer type (replication/migration/invalidation) in transfers table
* VFS-4000 Fixed fetching wrong transfer statistics for chosen timespan
* VFS-3956 Fixed provider name tooltip rendering in migrate menu of data distribution modal
* VFS-3595 Fixed locking ACL edit when switching between ACL and POSIX in permissions modal
* VFS-3591 Fixed infinite loading of metadata panel when failed to fetch metadata for file
* VFS-3210 Fixed displaying long text in basic file metadata keys and values
* VFS-3880 Various improvements in transfers management and statistics display
* VFS-3752 Added transfers tab; added replication and migration options from data distribution
* VFS-3710 Using binary prefix units for displaying sizes (MiB, GiB, etc.)
* VFS-3455 Refactored reconnect Websocket modal
* VFS-3600 Refactored file distribution modal
* VFS-3561 Fixed crash of spaces and groups view when new one is created
* VFS-3560 Fixed lack of handle-public relation in model when a share is public
* VFS-3402 Fixed redirection from login route when authenticated
* VFS-3421 Showing no permissions to view directory message when necessary
* VFS-3303 Fix files progress upload indicator when uploading empty file (0 bytes) 


### 17.06.0-rc2

* No changes in GUI since 17.06.0-rc1


### 17.06.0-rc1

* No changes in GUI since 17.06.0-beta6


### 17.06.0-beta1 - 17.06.0-beta6

* VFS-3068 Fixing bug: do not stuck on finalizing file upload after all files upload failure
* VFS-3414 Fixing modals button actions
* VFS-3390 Improved names truncating and tooltips for long names


### 3.0.0-rc15

* VFS-3172 Display Provider software version


### 3.0.0-rc13

* VFS-3126 Fixing share info modal infinite loader
* HOTFIX Fixing date display in file modification date
* VFS-3004 Upgrading Ember to 2.11; upgrading dependencies


### 3.0.0-rc12

* VFS-2968 Fixing hanging "Waiting for Onezone session" message
* VFS-2969 Improved performance of files list
* VFS-2943 Improved design of provider name in top
* VFS-2661 Major refactor of model; using User model with resources relations instead of findAll
* VFS-2921 Fixing missing file after upload (finalizing file upload after RPC completion)
* VFS-2417 Moved common op-gui and oz-gui features to ember-cli-onedata-common addon; major refactoring
* VFS-2865 Fixing metadata panel showing up
* VFS-2820 Provider name in top bar
* VFS-2841 Use app-config.json for runtime webapp config (used for debug messages enable/disable)
* VFS-2595 Showing conflicting files in GUI
* VFS-2834 Select files range with shift
* VFS-2754 Word wrap in modals
* VFS-2753 New upload indicator on top of files list; changed files list behaviour on upload
* VFS-2754 Refactor of files ACL and permissions modal; fixing issues with ACL changes
* VFS-2711 Rename files button and modal
* VFS-2752 Fixing actions on uncollapsed toolbar
* VFS-2752 Collapsible files list toolbar
* VFS-2752 Fixed hang of loading state when clicked multiple times on submit button in rename modal
* VFS-2752 Fixing some issues with top bar height
* VFS-2752 "No users" message in group users table
* VFS-2752 Files tree loading indicator
* VFS-2752 Files list loader on public share view
* VFS-2752 Updating Ember to 2.8.3
* VFS-2752 Fixing typo in translation
* VFS-2770 Dynamic file breadcrumbs component
* VFS-2697 Fixed height of uploader with scrollbars
* VFS-2767 Fixing infinite data-space loading when one of supporting providers does not work or there are no providers at all


### 3.0.0-rc5 -- 3.0.0-rc11

* VFS-2698 Spinners on loading users/groups names
* VFS-2698 Refactored spaces index go to default
* VFS-2697 Cleaned code and fixed tests
* VFS-2697 Clearing ResumableJS files on complete - Slight style change of file upload to not allow progress bar to disappear - Dynamic visible property - Experimental upload of dir files in Chrome - Refactored error-notifier service (currently unused) - Allow upload of 0-sized files - Reverting ResumableJS as singleton instance
* VFS-2697 Major refactoring of files upload comp.
* VFS-2494 Non-uppercase space name in selector
* VFS-2494 Fixing "dynamic count" in remove files
* VFS-2494 Fixing too small size column
* VFS-2494 Removing files: i18n, selected files count
* VFS-2494 One notify after removing multiple files
* VFS-2628 Going to default data space after render
* VFS-2628 Going to default data-space in data/index
* VFS-2660 Clearing login redirect timeout
* VFS-2660 Safer files row width update
* VFS-2660 Removing/modifying some debug messages
* VFS-2660 Added missing non-backend session change
* VFS-2660 Fixing some issues in app without app
* VFS-2658 Refactored upload batch to use service
* VFS-2660 Stub of new files list upload loader
* VFS-2660 New route for shared dir: `/*/shares/:share_id/:shared_dir_id`
* VFS-2658 Do not make explicit find on created file
* VFS-2660 Added more loading routes for data
* VFS-2660 Changed shares browser to get non-proxied File
* VFS-2660 Optional solid background in common loader
* VFS-2660 Handling missing file on find after creation
* VFS-2660 Added missing jsdoc
* VFS-2660 Unbinding events in truncated-string
* VFS-2658 Showing "more files pending" loader before first file arrives
* VFS-2658 Fixing some issues with racing fetchMoreFiles resolve and push
* VFS-2658 Make fixed space for fetch more files loader
* VFS-2660 Added loader for application
* VFS-2660 Safer breadcrumbs loader
* VFS-2658 Preventing more files load at file browser init
* VFS-2658 Fixing lack of "more files in progress" loader
* VFS-2660 Increasing level of root children in files tree
* VFS-2660 Fixing view change on removing last or active share
* VFS-2660 Fixes in files selection
* VFS-2660 Change click events on files list handling: use ctrl to select
* VFS-2660 Moving computeFileLabelMaxWidth to computed property
* VFS-2660 Better performance of truncating file names; bugfixing
* VFS-2658 Use publicRPC when fetching more files for public view
* VFS-2658 Fixing bugs in data files list loaders; refactoring
* VFS-2660 Data files list refactoring
* VFS-2658 Added fileModelType to RPC fetchMoreDirChildren
* VFS-2658 Improvements in files loading
* VFS-2660 Fixing random long session loading
* VFS-2660 Breadcrumbs in data view
* VFS-2658 New loader in files list; new file blink
* VFS-2658 Do not allow "preload ahead index" to be lesser than 0
* VFS-2658 Do not sort files in front-end
* VFS-2658 Fixing createFile implementation
* VFS-2658 Fixing createFile invocation
* VFS-2558 Do not fetch new files if loading dir
* VFS-2658 Rename all occurrences of fetchMoreChildren
* VFS-2658 Rename fetchMoreChildren RPC call
* VFS-2658 Dynamically load new files on scroll
* VFS-2660 Thicker icon for metadata
* VFS-2660 Bind secondary sidebar resize event to truncated string resize
* VFS-2660 Completed resizable files tree
* VFS-2660 Secondary sidebar classes refactoring and fixing sidebar width
* VFS-2660 Refactoring of data-files-tree
* VFS-2660 Fixes in auto-positioning resize handler
* VFS-2660 Cleaning up obsolete "data-files-tree service API"
* VFS-2660 Cleaning rel dir with make clean
* VFS-2658 Draft of files paging (dynamic load) methods
* VFS-2660 Empty shares new graphics
* VFS-2660 Separate "resize handler" element (buggy!)
* VFS-2660 Initial support for resizing files tree sidebar
* VFS-2603 Fixing typos
* VFS-2603 Improved color on secondary sidebar item hover
* VFS-2603 Improved no handle services error handling
* VFS-2603 No shares message
* VFS-2603 Completed public shares with Dublin Core
* VFS-2603 Panels backgrounds
* VFS-2603 Publish share error handling
* VFS-2603 Showing share metadata Dublin Core after creation
* VFS-2603 Work on share handle
* VFS-2603 Initial publish share button and modal
* VFS-2603 Added missing Share -> Handle relation
* VFS-2603 Added handles models
* VFS-2462 Removing failing test
* VFS-2462 Forced reload of shared files on shared files list
* VFS-2462 Fixing missing metadata add in file row
* VFS-2462 Disable metadata edition on public view
* VFS-2462 Create metadata in file browser file row
* VFS-2462 Using getSharedFileDownloadUrl RPC
* VFS-2462 Using containerDir in public shares - making it usable
* VFS-2462 Using file-shared model
* VFS-2462 Removing main metadata panel from shares show view
* VFS-2462 file-property-public; some comments and slight refactor
* VFS-2462 Improved metadata editor
* VFS-2462 Remove metadata button
* VFS-2462 Removing failing generated test
* VFS-2462 Fixing entering deleted share on shared index view
* VFS-2462 Fixing not working group name in groups table
* VFS-2462 Fixing multiple renames when clicking on button (Firefox)
* VFS-2462 Fixing ws-adapter updateRecord by not using serializeIntoHash
* VFS-2462 Using new ws-serializer
* VFS-2462 Added ws-serializer and fixing ws-adapter updateRecord
* VFS-2462 Added missing hasPrivilege support to group; bugfixing
* VFS-2462 Support for space's model "hasViewPrivilege"
* VFS-2462 Close metadata editor on discarding changes on non-persisted
* VFS-2462 Using object-string transform in metadata model and panel
* VFS-2462 Fixing WS Adapter's update record: use serializer
* VFS-2462 New experimental transform: object-string
* VFS-2462 Changed way how metadata is created with edit metadata button
* VFS-2462 Metadata editor work snapshot (styles, component fixes)
* VFS-2462 Using fixed metadata icon
* VFS-2462 Update oneicons (fix for metadata icon)
* VFS-2462 Metadata editor work snapshot
* VFS-2407 Fix spaces perms for group table
* VFS-2407 Added missing view files header; changing order of permissions
* VFS-2407 Added "write files" space permission
* VFS-2407 Added `test_rel_xunit_output` for testing production builds
* VFS-2462 Stub of broken metadata editor on data-files-list
* VFS-2462 First version of metadata editor with save
* VFS-2462 Fixing changes file-metadata to meta
* VFS-2407 Disable autofocus of input-copy in shares views
* VFS-2407 Fixing CSS bug for Firefox
* VFS-2462 Stub of metadata backend support
* VFS-2407 Max width of copy input in shares
* VFS-2047 Close create share modal on error
* VFS-2407 Own implementation of controller.isActive
* VFS-2407 Delete share for file on destroyRecord; clean-up
* VFS-2407 Clean-up, copy input for public url share
* VFS-2407 Styles for public shares view and shares style improvements
* VFS-2407 Added missing `_super` in login route setupController
* VFS-2407 Public mode download
* VFS-2407 Settings drop for Shares; settings-drop refactoring
* VFS-2407 Fixed some issues with goint to default/first obj. in indexes
* VFS-2407 JSDoc, cleaning up and bugfixing
* VFS-2582 Settings packages versions to fixed values
* VFS-2407 Fixing issues in OP login route; add. area for common-loader
* VFS-2407 Use share root dir as a root in breadcrumbs
* VFS-2407 Fixes for updated shares backend
* VFS-2581 Fixing page titles for new ember-page-title version
* VFS-2407 Breadcrumbs component
* VFS-2407 New spinner size
* VFS-2407 Shares view with bereadcrumbs snapshot
* VFS-2407 Stub of shares styled view with metadata
* VFS-2407 Remove join from shares menu
* VFS-2407 Read-only input copy
* VFS-2407 Spin-spinner for share info modal
* VFS-2407 Additional styles for spinner
* VFS-2407 input-copy component; copy button for share url
* VFS-2407 Fallback route for shares
* VFS-2407 Do not select the file when clicking on share tool
* VFS-2407 Fixing bugs; properly highlight share in shares menu
* VFS-2407 Create share on enter press
* VFS-2407 Disabling upload area in shares view
* VFS-2407 Update shares icons
* VFS-2478 Draft of breadcrumbs
* VFS-2478 Draft of go up button in shares file browser
* VFS-2478 Link to public view in shares show
* VFS-2478 Share public and bugfixing
* VFS-2478 Stub of public shares view; bugfixing
* VFS-2478 New route prefixes: onedata and public
* VFS-2407 Fixing info on Share summary
* VFS-2407 Fixed showing shared icons on files list
* VFS-2407 File browser in Share; using session-core (VFS-2478)
* VFS-2407 Using dataSpaceId in createShare RPC
* VFS-2407 Using folder-share icon; share summary tool on file
* VFS-2407 Shares view work snapshot: create and summary modals
* VFS-2407 Model simplification; dirsPath as a (auto-updated) property
* VFS-2407 Added menu-shared icon
* VFS-2407 Draft of Shared view and listing of shares; share model mock
* VFS-2407 Draft of file share modal and model


### 3.0.0-rc5

* VFS-2508 Do not show deleted groups; do not redirect to deleted groups
* VFS-2490 Removing development code
* VFS-2490 When no default space, load first sorted; jshint fix
* VFS-2490 Automatically update related space/data-space on records save
* VFS-2490 Using default behaviour in ember-data for should*Reload*
* VFS-2490 Fixes in redirecting to data space
* VFS-2490 Work snapshot: Go to first space
* Squashed 'gui/' changes from ecec512..2cd7150
* VFS-2487 Do not show files list when all files are broken
* VFS-2311 Using iframe to download file
* VFS-2487 Do not show "broken" files in files list
* VFS-2485 Fixing test environment for Ember 2.7
* Squashed 'gui/' changes from 3decbe5..ecec512
* VFS-2485 Making ws-adapter "query" compatible with ember-data 2.x
* VFS-2485 Migration to Ember 2.7; cleaning up code
* VFS-2311 Fix typo in oneproviderServer docs
* VFS-2311 Download promise reject on invalid backend RPC resolve
* VFS-2311 Using RPC to get file download URL; file row in list refactor
* VFS-2311 Handling fileDownloadError server message
* VFS-2311 Server push message support with server-messages-handler
* VFS-2304 Updating references to gui files
* Squashed 'gui/' changes from be7c162..3decbe5
* VFS-2304 Fixing files tree highlight on open in browser




________

Generated by sr-release. 
