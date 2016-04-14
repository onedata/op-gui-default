#!/usr/bin/env bash

find . -type l -exec sh -c "file -b {} | grep -q ^broken" \; -print