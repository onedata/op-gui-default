/**
 * Returns space/data-space that should be loaded by default
 * when entering index of spaces or data-spaces.
 * @return {Space|DataSpace}
 */
export default function(spaces) {
  let selectedSpace;
  let defaultSpace = spaces.find((s) => s.get('isDefault'));
  if (defaultSpace) {
    selectedSpace = defaultSpace;
  } else {
    console.debug('No default data-space found - go to first data-space instead');
    const firstSpace = spaces.sortBy('name').objectAt(0);
    if (firstSpace) {
      selectedSpace = firstSpace;
    } else {
      console.debug('get-default-space: no data-spaces exist');
    }
  }
  return selectedSpace;
}