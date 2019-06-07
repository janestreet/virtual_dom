// Used by workaround for input element value field
// Based on https://github.com/Matt-Esch/virtual-dom/blob/947ecf92b67d25bb693a0f625fa8e90c099887d5/virtual-hyperscript/hooks/soft-set-hook.js

joo_global_object.SoftSetHook = function(value) {
    if (!(this instanceof SoftSetHook)) {
        return new SoftSetHook(value);
    }

    this.value = value;
};

joo_global_object.SoftSetHook.prototype.hook = function (node, propertyName) {
    if (node[propertyName] !== this.value) {
        node[propertyName] = this.value;
    }
};


var GenericHook = function (init, update, destroy, id) {
    if (!(this instanceof GenericHook)) {
        return new GenericHook(init, update, destroy, id);
    }

    this.init = init;
    this.update = update;
    this.destroy = destroy;
    this.id = id;
};

GenericHook.canTransition = function (from, to) {
    return from instanceof this && to instanceof this && from.id === to.id && to.update;
};

GenericHook.prototype.hook = function (node, propName, prev) {
    if (GenericHook.canTransition(prev, this)) {
        this.state = this.update(prev.state, node);
    } else {
        this.state = this.init(node);
    }
};

GenericHook.prototype.unhook = function (node, propName, next) {
    if (GenericHook.canTransition(this, next)) {
        // Do nothing, the impending [hook] will handle the call to update.
    } else {
        this.destroy(this.state, node);
    }
};

joo_global_object.GenericHook = GenericHook;
