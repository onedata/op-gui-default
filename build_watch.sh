#!/usr/bin/env bash

SRC_DIR=src
REL_DIR=rel

cd ${SRC_DIR} && ember build --watch --output-path=../${REL_DIR}