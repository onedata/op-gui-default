import RESTSerializer from 'ember-data/serializers/rest';

/**
 * A serializer for Onedata WebSocket adapter.
 * Extends default ``RESTSerializer``.
 *
 * @module adapters/ws-serializer
 * @author Jakub Liput
 * @copyright (C) 2016 ACK CYFRONET AGH
 * @license This software is released under the MIT license cited in 'LICENSE.txt'.
 */
export default RESTSerializer.extend({
  /**
   * Extended implementation of json's serializer serialize.
   * Added optional ``option.keys`` which is an Array of attribute keys (String)
   * If ``option.keys`` is present, only attributes with these keys will be serialized
   */
  serialize(snapshot, options) {
    var json = {};

    if (options) {
      if (options.includeId) {
        var id = snapshot.id;

        if (id) {
          json[this.get('primaryKey')] = id;
        }
      }
    }

    snapshot.eachAttribute((key, attribute) => {
      if (options.keys ? options.keys.includes(key) : true) {
        this.serializeAttribute(snapshot, json, key, attribute);
      }
    });

    snapshot.eachRelationship((key, relationship) => {
      if (options.keys ? options.keys.includes(key) : true) {
        if (relationship.kind === 'belongsTo') {
          this.serializeBelongsTo(snapshot, json, relationship);
        } else if (relationship.kind === 'hasMany') {
          this.serializeHasMany(snapshot, json, relationship);
        }
      }
    });

    return json;
  },
});
