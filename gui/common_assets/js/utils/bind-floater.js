// jshint esnext: true

/**
  Makes an element fixed positioned, with coordinates (left, top) of its parent.
  Please use returned function to recompute fixed position on your events (eg. scroll).

  @param element A jQuery element which will float (only single!)
  @param parent (optional) A jQuery element which will act as element position parent.
          If not provided - will use a element.parent().
  @param {object} options Additional options to manipulate floater behaviour, properties:
            - posX {string} right/left - the floater will position on right/left of parent
                (default: right)
            - posX {string} top/middle - the floater (0,0) will position on top/middle of parent
                (default: top)
            - offsetX {integer} - X offset of (0,0) position in px
            - offsetY {integer} - X offset of (0,0) position in px
  @returns {function} Function which re-computes new fixed position of an element.
              It should be bind to eg. parent mouseover.
 */
export default function bindFloater(element, parent, options) {
  parent = parent || element.parent();
  // default options
  options = options || {};
  options.posX = options.posX || 'right';
  options.posY = options.posY || 'top';
  options.offsetX = options.offsetX || 0;
  options.offsetY = options.offsetY || 0;

  element.addClass('floater');
  let changePos = function() {
    let offset = parent.offset();
    let left;
    if (options.posX === 'right') {
      left = `${parseInt(offset.left) + parent.width()}px`;
    } else if (options.posX === 'left') {
      left = `${parseInt(offset.left) - element.width()}px`;
    }
    let top;
    if (options.posY === 'top') {
      top = offset.top;
    } else if (options.posY === 'middle') {
      top = `${parseInt(offset.top) - element.height()/2}px`;
    }

    element.css({
      left: `${parseInt(left) + options.offsetX - $(window).scrollLeft()}px`,
      top: `${parseInt(top) + options.offsetY + $(window).scrollTop()}px`
    });
  };

  changePos();
  return changePos;
}
