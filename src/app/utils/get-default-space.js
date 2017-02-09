/**
 * Returns a space that should be loaded by default
 * when entering index of spaces or data-spaces.
 * @return {Space}
 */
export default function(spaces) {
  let selectedSpace;
  let defaultSpace = spaces.find((s) => s.get('isDefault'));
  if (defaultSpace) {
    selectedSpace = defaultSpace;
  } else {
    console.debug('util:get-default-space: No default data-space found - choose data-space instead');
    const firstSpace = spaces.sortBy('name').objectAt(0);
    if (firstSpace) {
      selectedSpace = firstSpace;
    } else {
      console.debug('util:get-default-space: No data-spaces exist');
    }
  }
  return selectedSpace;
}