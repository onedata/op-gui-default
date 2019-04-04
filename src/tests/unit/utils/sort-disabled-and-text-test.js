import { expect } from 'chai';
import { describe, it } from 'mocha';
import sortDisabledAndText from 'op-worker-gui/utils/sort-disabled-and-text';

describe('Unit | Utility | sort disabled and text', function () {
  it('sorts all by name and all disabled goes to the end of list', function () {
    const list = [
      { text: 'common', disabled: true },
      { text: 'zeta', disabled: true },
      { text: 'bonanza', disabled: false },
      { text: 'abacus', disabled: true },
      { text: 'banana', disabled: false },
      { text: 'awkward', disabled: false },
    ];

    list.sort(sortDisabledAndText);

    expect(list.mapBy('text')).to.deep.equal(
      [
        'awkward',
        'banana',
        'bonanza',
        'abacus',
        'common',
        'zeta',
      ]
    );
  });

  it('is not case sensitive', function () {
    const list = [
      { text: 'Common', disabled: false },
      { text: 'zeta', disabled: false },
      { text: 'Bonanza', disabled: false },
      { text: 'banana', disabled: false },
    ];

    list.sort(sortDisabledAndText);

    expect(list.mapBy('text'), list.mapBy('text')).to.deep.equal(
      [
        'banana',
        'Bonanza',
        'Common',
        'zeta',
      ]
    );
  });
});
