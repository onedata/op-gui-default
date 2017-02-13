import DS from 'ember-data';
import createFilePropertyModel from 'op-worker-gui/mixin-factories/models/file-property';

export default DS.Model.extend(createFilePropertyModel('regular'));
