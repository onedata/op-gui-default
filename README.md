About
=====

*op-gui-default* is GUI written in Ember distributed with OP worker.

**Note: this is legacy version of web GUI used in Onedata op-worker version 19.02.x
and it is not longer maintained. Newer version use `oneprovider-gui`.**

You can find `oneprovider-gui` in the repositories:
- https://github.com/onedata/oneprovider-gui (publicly available repo)
- https://git.onedata.org/projects/VFS/repos/oneprovider-gui (internal development repo, private)

Goals
-----

This repo allows to separate GUI from OP worker repo, which improves
ease of maintenance and lowers build times.

It is able to build a release containing only compiled GUI static files
and create a static docker with those files.

Getting Started
---------------

To build a development release of GUI:

```
make             # run natively (requires npm, bower, ember)
```
```
make submodules
./make.py -i onedata/gui_builder:v14    # run in docker (onedata/builder) that has all deps
```
<br />

To build a production release of GUI:

```
make rel         # run natively (requires npm, bower, ember)
```
```
make submodules
./make.py -i onedata/gui_builder:v14 rel    # run in docker (onedata/builder) that has all deps
```
<br />

To package a built release into a static docker:

```
./sr-dockerbuild
```
<br />

To package a built release into a static docker and
push it to docker repo:

```
./sr-dockerbuild --push --remove
```
<br />

To get more info on ****sr-dockerbuild.sh**** usage:

```
./sr-dockerbuild --help
```
<br />

To start an ember build process that watches for changes and rebuilds:

```
./build_watch.sh
```
<br />

To find all broken symbolic links (those that point to nothing):

```
./find_broken_links.sh
```
<br />

Testing and publishing on CI (eg. Bamboo)
-----------------------------------------

To run tests on CI use:

```
make submodules
./make.py -i onedata/gui_builder:v14 test_xunit_output
```

Results of tests will be available in: `src/tests/test-results.xml` (use JUnit Parser
on Bamboo to parse tests).

To build docker image and publish it use:

```
./bamboos/scripts/publish_gui_pkg_docker.sh --user <docker_user> --password <docker_password>
```