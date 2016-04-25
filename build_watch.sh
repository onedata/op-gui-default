#!/usr/bin/env bash

#####################################################################
# @author Lukasz Opiola
# @copyright (C): 2016 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# usage:
# ./build_watch.sh
#
# This script starts an ember build process that watches for changes in
# src dir and automatically rebuilds GUI release.
#####################################################################

SRC_DIR=src
REL_DIR=rel

cd ${SRC_DIR} && ember build --watch --output-path=../${REL_DIR}
