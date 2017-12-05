/**
 * Creates value similiar to belongsTo relation
 * (used mainly for testing with fake models) 
 *
 * @module utils/mock-belongs-to
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import PromiseObject from 'ember-cli-onedata-common/utils/ember/promise-object';

const {
  RSVP: { Promise },
} = Ember;


export default function mockBelongsTo(obj) {
  return PromiseObject.create({
    promise: Promise.resolve(obj),
  });
}
