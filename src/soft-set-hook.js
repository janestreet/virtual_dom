// Used by workaround for input element value field
// Based on https://github.com/Matt-Esch/virtual-dom/blob/947ecf92b67d25bb693a0f625fa8e90c099887d5/virtual-hyperscript/hooks/soft-set-hook.js

joo_global_object.SoftSetHook = function(value) {
    if (!(this instanceof SoftSetHook)) {
        return new SoftSetHook(value);
    }

    this.value = value;
}

joo_global_object.SoftSetHook.prototype.hook = function (node, propertyName) {
    if (node[propertyName] !== this.value) {
        node[propertyName] = this.value;
    }
};
