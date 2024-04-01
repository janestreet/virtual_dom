"Virtual_dom: a virtual DOM diffing library"
============================================

This library is an OCaml wrapper of Matt Esch's [virtual-dom library](https://github.com/Matt-Esch/virtual-dom).
It provides a simple, immutable representation of a desired state of
the DOM, as well as primitives for updating the real DOM in the
browser to match that, both by slamming the entire DOM in place, and
by computing diffs between successive virtual-DOMs, and applying the
resulting patch to the real DOM.

It has been vendored internally as the external library has gone unsupported for many
years and we needed some modifications.

# Contributing

Please exercise a high degree of caution if you modify this library. Our testing
infrastructure doesn't support robust browser testing, so any changes here should be
_thoroughly_ smoke tested on a variety of apps with different usecases and potential DOM
~ab~uses. We intend this code to be as stable/unchanging as possible because of the
difficult testing situation and near ubiquitous use in web apps at Jane Street.
