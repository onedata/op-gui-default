import DS from 'ember-data';

/**
 * Information about distribution of file blocks among single provider.
 * @module models/file-distribution
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default DS.Model.extend({
  provider: DS.belongsTo('provider', {async: true}),

  /**
    Array of integers. Number of elements should be even.
    - Let n=0,2,4,6,...
    - Each (n, n+1) pair of integers is a file block (start, end) bytes count.
    - Eg. [0,10,15,20,60,100] denotes, that there are 3 file blocks:
      - 0-10b, 15-20b, 60-100b
    - the file (which has a id = fileId) should have size >= last number (in this example >= 100b)
  */
  blocks: DS.attr(),

  fileId: DS.attr('string')
});
