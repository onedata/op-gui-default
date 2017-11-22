import Ember from 'ember';

const {
  String: { camelize },
} = Ember;

export function registerService(testCase, name, stub) {
  testCase.register(`service:${name}`, stub);
  return testCase.inject.service(name, { as: camelize(name) });
}

export function lookupService(testCase, name) {
  return testCase.container.lookup(`service:${name}`);
}
