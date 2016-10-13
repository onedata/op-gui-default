#!/bin/bash

#####################################################################
# @author Jakub Liput
# @copyright (C): 2016 ACK CYFRONET AGH
# This software is released under the MIT license
# cited in 'LICENSE.txt'.
#####################################################################
# Copies built static files into launched getting-started container
# Usage: deploy_to_getting_started.sh <op-worker/oz-worker>
#####################################################################

WORKER_TYPE=$1
if [ "$WORKER_TYPE" = "op-worker" ]; then
    DOCKER_DIR="op_worker"
    DOCKER_NAME="oneprovider-1"
elif [ "$WORKER_TYPE" = "oz-worker" ]; then
    DOCKER_DIR="oz_worker"
    DOCKER_NAME="onezone-1"
else
    echo "Usage: gui/scripts/deploy_to_getting_started.sh <op-worker/oz-worker>"
    echo "Launch from repo root dir"
    exit 1
fi

docker exec ${DOCKER_NAME} bash -c "rm -rf /var/lib/${DOCKER_DIR}/gui_static/*"
pushd rel
    for NAME in `find .`; do
    DEST=/var/lib/${DOCKER_DIR}/gui_static/${NAME}
    echo "copy ${NAME} to ${DEST}"
        docker cp $NAME ${DOCKER_NAME}:${DEST}
    done
popd
