SRC_DIR	 ?= src
REL_DIR	 ?= rel

.PHONY: deps build_dev build_prod doc clean test

all: build_dev

rel: build_prod

deps:
	cd $(SRC_DIR) && npm install
	cd $(SRC_DIR) && bower install --allow-root

build_dev: deps
	cd $(SRC_DIR) && ember build -dev --output-path=../$(REL_DIR)

build_prod: deps
	cd $(SRC_DIR) && ember build -prod --output-path=../$(REL_DIR)

doc:
	jsdoc -c $(SRC_DIR)/.jsdoc.conf $(SRC_DIR)/app

clean:
	cd $(SRC_DIR) && rm -rf node_modules bower_components dist tmp

test:
	cd $(SRC_DIR) && ember test
