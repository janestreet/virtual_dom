var vdom = {
  VNode: require('./vendor/vnode/vnode.js'),
  VText: require('./vendor/vnode/vtext.js'),
  diff: require('./vendor/diff.js'),
  patch: require('./vendor/patch.js'),
  createElement: require('./vendor/create-element.js'),
  svg: require("./vendor/virtual-hyperscript/svg.js"),
};

global.VirtualDom = vdom;
module.exports = vdom;
