/* global Chartist */

import Ember from 'ember';
import _ from 'lodash';
import moment from 'moment';
import generateColors from 'op-worker-gui/utils/generate-colors';
import legendColors from 'op-worker-gui/utils/chartist/legend-colors';

const {
  Component,
  computed,
  get,
} = Ember;

export default Component.extend({
  /**
   * @type {Object}
   * @virtual
   */
  stats: undefined,

  lastUpdateTime: undefined,
  
  /**
   * One of `minute`, `hour`, `day`.
   * @type {string}
   * @virtual
   */
  timeUnit: 'minute',

  _chartValues: [],

  /**
   * @type {Ember.ComputedProperty<Object>}
   */
  _statsForTimeUnit: computed('stats', 'timeUnit', function () {
    const {
      stats,
      timeUnit,
    } = this.getProperties('stats', 'timeUnit');
    return get(stats, timeUnit);
  }),

  _sortedProvidersIds: computed('_statsForTimeUnit', function () {
    return Object.keys(this.get('_statsForTimeUnit')).sort();
  }),

  _providersColors: computed('_sortedProvidersIds', function () {
    return generateColors(this.get('_sortedProvidersIds').length);
  }),

  _sortedStatsValues: computed('_sortedProvidersIds', function () {
    const {
      _statsForTimeUnit,
      _sortedProvidersIds,
    } = this.getProperties('_statsForTimeUnit', '_sortedProvidersIds');
    return _sortedProvidersIds.map(providerId =>
      _statsForTimeUnit[providerId] || []
    );
  }),

  _timePeriod: computed('timeUnit', function () {
    const timeUnit = this.get('timeUnit');
    switch (timeUnit) {
      case 'minute':
        return [12, 'seconds'];
      case 'hour':
        return [12, 'minutes'];      
      default:
      case 'day':
        return [2, 'hours'];
    }
  }),
  
  _timeFormat: computed('timeUnit', function () {
    switch (this.get('timeUnit')) {
      case 'hour':
        return 'HH:mm';
      case 'day':
        return 'DD/MM HH:mm';
      default:
        return 'HH:mm:ss';
    }
  }),

  /**
   * Chartist settings
   * @type {Object}
   */
  _chartOptions: computed('_providersColors', function() {
    const _providersColors = this.get('_providersColors');
    return {
      axisY: {
        onlyInteger: true,
      },
      low: 0,
      stackBars: true,
      chartPadding: 30,
      plugins: [
      //   maximizeBarWidth(),
      //   additionalXLabel(),
      //   barSumLabels(),
      //   tooltip({
      //     chartType: 'bar',
      //     rangeInTitle: true,
      //     renderAboveBarDescription: true,
      //   }),
      //   axisLabels({
      //     xLabel: 'Time',
      //     yLabel: 'Files',
      //   }),
      //   shortHorizontalGrid(),
        Chartist.plugins.legend(),
        legendColors({
          colors: _providersColors,
        }),
      //   refreshLegendFilter(),
      ],
    };
  }),

  /**
   * Data for chartist
   * @type {computed.Object}
   */
  _chartData: computed('_sortedStatsValues.[]', function () {
    let {
      _sortedStatsValues,
      _sortedProvidersIds,
      _chartValues,
    } = this.getProperties('_sortedStatsValues', '_sortedProvidersIds', '_chartValues');
    if (_sortedStatsValues && _sortedStatsValues.length > 0) {
      while (_sortedStatsValues.length > _chartValues.length) {
        _chartValues.push([]);
      }
      const statsValues = _sortedStatsValues.slice(1);
      statsValues.forEach((values, index) => {
        while (_chartValues[index].length) {
          _chartValues[index].shift();
        }
        values.forEach(value => _chartValues[index].push(value));
      });
      return {
        labels: _.range(1, _chartValues[0].length + 1).reverse()
          .map(n => this.getChartLabel(n)),
        series: _chartValues.map((values, index) => {
          return {
            name: _sortedProvidersIds[index], // TODO get name of the provider
            data: _chartValues[index],
            className: `ct-series-${index}`,
          };
        }),
        lastLabel: this.getChartLabel(0),
      };
    } else {
      return {};
    }
  }),

  init() {
    this._super(...arguments);
    this.set('_chartValues', []);
  },

  getChartLabel(offset) {
    let {
      lastUpdateTime,
      _timeFormat,
      _timePeriod,
    } = this.getProperties(
      'lastUpdateTime',
      '_timeFormat',
      '_timePeriod');
    return moment(lastUpdateTime)
      .subtract(offset * _timePeriod[0], _timePeriod[1])
      .format(_timeFormat);
  },
});
