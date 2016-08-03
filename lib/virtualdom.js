var vdom = {
  VNode: require('virtual-dom/vnode/vnode'),
  VText: require('virtual-dom/vnode/vtext'),
  diff: require('virtual-dom/diff'),
  patch: require('virtual-dom/patch'),
  createElement: require('virtual-dom/create-element'),
  svg: require("virtual-dom/virtual-hyperscript/svg"),
};

global.VirtualDom = vdom;
module.exports = vdom;
