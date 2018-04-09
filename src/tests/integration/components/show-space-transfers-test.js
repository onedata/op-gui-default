// FIXME: these tests requires currentTransferList and completedTransferList
// to be available in space model; change component code or tests code

// import { expect } from 'chai';
// import { describe, it, beforeEach } from 'mocha';
// import { setupComponentTest } from 'ember-mocha';
// import hbs from 'htmlbars-inline-precompile';
// import Ember from 'ember';
// import { registerService } from '../../helpers/stub-service';
// import mockHasMany from 'op-worker-gui/utils/mock-has-many';
// import wait from 'ember-test-helpers/wait';

// const {
//   Service,
// } = Ember;

// const PROVIDER_ID = 'p1';

// const SessionStub = Service.extend({
//   sessionDetails: {
//     providerId: PROVIDER_ID,
//   }
// });

// describe('Integration | Component | show space transfers', function() {
//   setupComponentTest('show-space-transfers', {
//     integration: true
//   });

//   beforeEach(function () {
//     registerService(this, 'session', SessionStub);
//   });
    
//   it('renders warning message if space is not supported by session provider', function() {
//     const space = {
//       id: 's1',
//       providerList: {
//         queryList: mockHasMany(['p2', 'p3'])
//       },
//       currentTransferList: {
//         content: {
//           hasMany() {
//             return {
//               ids() {
//                 return [];
//               }
//             };
//           },
//         },
//       },
//     };
//     this.set('space', space);
    
//     this.render(hbs`
//     <div id="content-scroll">
//       {{show-space-transfers space=space transfersUpdaterEnabled=false}}
//     </div>
//     `);
    
//     return wait().then(() => {
//       expect(this.$('.error-space-not-supported'), 'error message renders')
//         .to.exist;
//     });
//   });
  
//   it('renders loading spinner if space support by provider is not yet defined', function() {
//     const space = {
//       id: 's1',
//       providerList: {
//         queryList: mockHasMany(['p2', 'p3'])
//       },
//       currentTransferList: {
//         content: {
//           hasMany() {
//             return {
//               ids() {
//                 return [];
//               }
//             };
//           },
//         },
//       },
//     };
//     this.set('space', space);
    
//     this.render(hbs`
//     <div id="content-scroll">
//       {{show-space-transfers space=space transfersUpdaterEnabled=false}}
//     </div>
//     `);
    
//     expect(
//       this.$('.row-transfers-data-container > .spin-spinner-block'),
//       'loading spinner renders'
//     ).to.exist;
//   });
// });
