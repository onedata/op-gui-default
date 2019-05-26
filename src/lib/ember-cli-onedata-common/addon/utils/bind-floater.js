/**
  Makes an element fixed positioned, with coordinates (left, top) of its parent.

  @param element A jQuery element which position will be computed in relation to "parent"
  @param {object} [parent=element.parent()] A jQuery element which will act as element position parent.
          If null or undefined - will use a element.parent().
  @param {object} options Additional options to manipulate floater behaviour, properties:
  @param {string} [options.posX=right] horizontal position of element relative to the parent, possible:
            - ``left`` - the element will be placed completely on left of the parent
            - ``right`` - the element will be placed completely on right of the parent
  @param {string} [options.posY=top] vertical position of element relative to the parent, possible:
            - ``top`` - the element will be placed completely on top of the parent
            - ``top-above`` - same as ``top``
            - ``top-middle`` - the vertical middle of element will be placed on top of parent
            - ``middle`` - the vertical middle of element will be placed in the vertical middle of parent
            - ``middle-middle`` - same as ``middle``
            /middle - the floater (0,0) will position on top/middle of parent
                (default: top)
  @param {integer} [options.offsetX=0] X offset of computed position in px
  @param {integer} [options.offsetY=0] Y offset of computed position in px
  @param {jQuery} [options.stackingContext] A jQuery element which is a stacking context
    - effectively it will be a relative parent, see: https://stackoverflow.com/a/25828495

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
  options.stackingContext = options.stackingContext || 0;

  element.addClass('floater');
  let changePos = function() {
    let offset = parent.offset();
    let left;
    if (options.posX === 'right') {
      left = parseInt(offset.left) + parent.width();
    } else if (options.posX === 'left') {
      left = parseInt(offset.left) - element.width();
    } else if (options.posX === 'center') {
      left = parseInt(offset.left) + parent.width()/2 - element.width()/2;
    }
    let top;
    if (options.posY === 'top' || options.posY === 'top-above') {
      top = offset.top;
    } else if (options.posY === 'top-middle') {
      top = parseInt(offset.top) - element.height()/2;
    } else if (options.posY === 'middle' || options.posY === 'middle-middle') {
      top = parseInt(offset.top) + parent.height()/2 - element.height()/2;
    }

    let stackingContext = options.stackingContext;
    if (stackingContext) {
      if (!(stackingContext instanceof $)) {
        stackingContext = $(stackingContext);
      }
      const stackingOffset = stackingContext.offset();
      left -= stackingOffset.left;
      top -= stackingOffset.top;
    }
    
    element.css({
      left: `${left + options.offsetX - $(window).scrollLeft()}px`,
      top: `${top + options.offsetY + $(window).scrollTop()}px`
    });
  };

  changePos();
  return changePos;
}
