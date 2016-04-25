#!/usr/bin/env bash

#####################################################################
# @author Lukasz Opiola
# @copyright (C): 2016 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# usage:
# ./find_broken_links.sh
#
# This scripts finds all symbolic links that point to nothing.
#####################################################################

find . -type l -exec sh -c "file -b {} | grep -q ^broken" \; -print
