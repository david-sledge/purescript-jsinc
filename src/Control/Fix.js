"use strict"

// Y-combinator? Y-not-combinator!
export const fix = f => (x => f(y => (x(x))(y)))(x => f(y => (x(x))(y)));
