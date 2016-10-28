/* jshint expr:true */
import { expect } from 'chai';
import {
  describeComponent,
  it
} from 'ember-mocha';
import { mockI18n } from 'ember-i18n-test-helpers';

class FakeResumableFile {
  constructor(fileName, uuid) {
    this.fileName = fileName;
    this.uniqueIdentifier = uuid;
    this._progress = 0;
  }
  progress() {
    return this._progress;
  }
}

describeComponent(
  'file-upload',
  'FileUploadComponent',
  {
    // Specify the other units that are required for this test
    needs: [
      'service:fileUpload',
      'service:session',
      'service:eventsBus',
      'service:oneproviderServer',
      'service:i18n'
    ],
    unit: true,
    setup() {
      mockI18n().withDefault('text');
    }
  },
  function() {
    // FIXME: unresolved dependencies
    // it('renders', function() {
    //   // creates the component instance
    //   let component = this.subject();
    //   // renders the component on the page
    //   this.render();
    //   expect(component).to.be.ok;
    //   expect(this.$()).to.have.length(1);
    // });

    // checks: getUploadingFile, addOrGetUploadingFile, addUploadingFile methods
    it('can store UploadingFile for corresponding ResumableFile', function() {
      const F_UUID = 'dkasd9ahsdf7s8fgt67atgdefdus8fs9-fdsfij8ds9f';
      const F_NAME = 'test1';
      let component = this.subject();
      let fakeResumableFile = new FakeResumableFile(F_NAME, F_UUID);
      let firstGotUploadingFile = component.getUploadingFile(fakeResumableFile); 
      expect(firstGotUploadingFile).to.not.exist;

      let createdUploadingFile = component.addOrGetUploadingFile(fakeResumableFile);
      expect(createdUploadingFile).to.exist;
      expect(createdUploadingFile.get('resumableFile').fileName).to.equal(F_NAME);
      expect(component.get('uploadingFiles.length')).to.equal(1);

      let secondGet = component.getUploadingFile(fakeResumableFile);
      expect(secondGet).to.exist;
      expect(secondGet.get('resumableFile').fileName).to.equal(F_NAME);
      expect(component.get('uploadingFiles.length')).to.equal(1);

      let secondAddOrGet = component.addOrGetUploadingFile(fakeResumableFile);
      expect(secondAddOrGet).to.exist;
      expect(secondAddOrGet.get('resumableFile').fileName).to.equal(F_NAME);
      expect(component.get('uploadingFiles.length')).to.equal(1);
    });

    // FIXME: changed behaviour
    // it('should filter visible uploading files by progress', function() {
    //   let comp = this.subject();
    //   let rfiles = ['a', 'b', 'c'].map(name => new FakeResumableFile(name, name + '_id'));
      
    //   rfiles.forEach(rf => comp.addUploadingFile(rf));
    //   expect(comp.get('visibleUploadingFiles.length')).to.equal(0);

    //   // change progress manually and notify
    //   rfiles[0]._progress = 0;
    //   rfiles[1]._progress = 30;
    //   rfiles[2]._progress = 100;
    //   let onFileProgress = comp.get('onFileProgress'); 
    //   rfiles.forEach(rf => onFileProgress(rf));

    //   expect(comp.get('visibleUploadingFiles.length')).to.equal(1);
    // });
  }
);
