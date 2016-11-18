import Ember from 'ember';
import FileBreadcrumbsItem from 'op-worker-gui/utils/file-breadcrumbs-item';

const FakeFile = Ember.Object.extend({
  hasParent: Ember.computed('__parent', function() {
    return this.get('__parent') != null; 
  }),
  parent: Ember.computed(function() {
    return new Ember.RSVP.Promise(resolve => resolve(this.get('__parent')));
  })
});

function generateBreadcrumbsItems(numberOfFiles) {
  let result = {};
  result.fileNames = [...Array(numberOfFiles).keys()].map(i => `file-${i}`);
  result.files = result.fileNames.map(name => FakeFile.create({
    name: name,
    id: name + '-id'
  }));
  for (let i=0; i<result.files.length; i+=1) {
    /* jshint loopfunc: true */
    let ic = i;
    result.files[ic].set('__parent', result.files[ic-1]);
  }
  result.bitems = Ember.A(result.files.map(file => FileBreadcrumbsItem.create({
    file: file
  })));
  return result;
}

export default generateBreadcrumbsItems;
