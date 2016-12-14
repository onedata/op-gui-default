# ember-cli-onedata-common

## About

*ember-cli-onedata-common* is an EmberJS in-repo addon that contains components, services, styles
and other EmberJS application resources as well as static resources (images, fonts) for building
Onedata web applications:
- ``op-gui-default``
- ``oz-gui-default``
- ``oz-gui-homepage``

## Usage in projects

Use this repo as a subtree in Ember application ``lib`` directory, which in case of Onedata apps is placed in: ``src/lib/``.
If you want to modify this addon from specific Onedata project, do from project's root:
- add a remote: ``git remote add ember-cli-onedata-common git@github.com:kliput/ember-cli-onedata-common.git``
- pull recent changes: ``git subtree pull --squash --prefix=src/lib/ember-cli-onedata-common ember-cli-onedata-common develop`` (you can use other branch name than ``develop``)
- make changes in ``src/lib/ember-cli-onedata-common`` and commit them
- push changes to project's repo: ``git push``
- push changes to addon repo: ``git subtree push --squash --prefix=src/lib/ember-cli-onedata-common ember-cli-onedata-common <branch_name>``

## Organisation of files

The project has structure of standard Ember CLI in-repo addon. See: https://ember-cli.com/extending/#addon-project-structure  

## Provided functionality overview

The addon provides common Onedata web application components, helpers, services, etc.
Visit their standard addon directories to see their documentation.

One of the important parts of the addon is integration with GUI backend, which can be presented as a chain of dependent modules:

``authenticator:basic`` -> ``service:session`` -> ``service:session-core`` -> ``service:server`` -> ``adapter:onedata-socket``

These modules and other interesting parts of the addon are described in more details below.

### WebSocket adapter

Onedata front-end appliactions communicate with Onedata GUI backend via WebSockets using JSON messages.
The ``adapter:onedata-websocket`` provides:
- Ember adapter implementation (model handling),
- RPC calls (specific to Onedata, see ``RPC`` method and ``service:server``'s ``privateRPC`` and ``publicRPC``).

Currently ``adapter:onedata-websocket`` performs all "low-level" communication using WebSocket object.

### Server (communication with backend)

Currently is an facade for backend (WebSocket and RPC) and for "low-level" WebSocket operations uses ``adapter:onedata-websocket``. 

### Session

A session implementation for ``ember-simple-auth``. It consists of two modules:
- ``service:session-core`` - implements most of ``ember-simple-auth``'s session and stores main information about session; uses ``service:server``
- ``service:session`` - handles session events like connection started or connection lost; informs user about WS connection issues

### Ember Authenticator for ember-simple-auth 

All Onedata Ember web applications use ember-simple-auth (https://github.com/simplabs/ember-simple-auth).
This addon provides ``authenticator:basic`` which uses session state (see: ``service:session``) to resolve or reject session.

### Oneicons font

The ``oneicon`` font provides a set of icons used in Onedata front-end.
It is generated using Icomoon web application (https://icomoon.io). Source files are in SVG format, with size 128x128px, using only black color.
They are currently not published in any repository.

To use icons, you can import a ``onedata-common`` (all common onedata styles) SCSS file to your main SCSS or ``oneicons`` for only icons.

Settings of Icomoon for generating icons are:
- Font name: ``oneicons``
- Class prefix: ``icon-``
- Support IE8
- Generate stylesheet variables for: Sass
- CSS Selector: Use attribute selector
- Em square height: 1024
- Baseline height: 0
- Whitespace width: 0

Package generated with Icomoon is placed in ``public/assets/fonts/oneicon`` directory.
The directory contains also ``demo.html`` file for browsing icons.

Icons can be used with ``one-icon`` component (included in gui) or ``icon`` helper (only in ``op-gui-default``).
They can be also used in pure HTML:

```
<span class="oneicon oneicon-(icon-name)"></span>
```

### Tests

Currently, GUI front-end JS files have no tests in this repo - the tests can be found in specific projects that uses them.
