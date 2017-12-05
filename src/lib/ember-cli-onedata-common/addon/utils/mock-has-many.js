/**
 * Creates value similiar to belongsTo relation
 * (used mainly for testing with fake models)  
 *
 * @module utils/mock-has-many
 * @author Jakub Liput
 * @copyright (C) 2017 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */

import Ember from 'ember';
import PromiseArray from 'ember-cli-onedata-common/utils/ember/promise-array';

const {
  RSVP: { Promise },
  A,
} = Ember;

export default function mockHasMany(array) {
  return PromiseArray.create({
    promise: Promise.resolve(A(array))
  });
}
