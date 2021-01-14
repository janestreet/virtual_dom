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


var GenericHook = function (init, update, destroy, id, extra) {
    if (!(this instanceof GenericHook)) {
        return new GenericHook(init, update, destroy, id, extra);
    }

    this.init = init;
    this.update = update;
    this.destroy = destroy;
    this.id = id;
    this.extra = extra;
};

var hook_state_key = "vdom_hook_state_key";

if (this.Symbol) {
    hook_state_key = Symbol(hook_state_key);
}

GenericHook.write_state = function (node, propName, state) {
    if (!node[hook_state_key]) {
        node[hook_state_key] = {};
    }
    node[hook_state_key][propName] = state;
}

GenericHook.read_state = function (node, propName) {
    return node[hook_state_key][propName];
}

GenericHook.remove_state = function (node, propName) {
    delete node[hook_state_key][propName];
}

GenericHook.canTransition = function (from, to) {
    return from instanceof this && to instanceof this && from.id === to.id && to.update;
};

GenericHook.prototype.hook = function (node, propName, prev) {
    if (GenericHook.canTransition(prev, this)) {
        var state = GenericHook.read_state(node, propName);
        state = this.update(state, node);
        GenericHook.write_state(node, propName, state);
    } else {
        var state = this.init(node);
        GenericHook.write_state(node, propName, state);
    }
};

GenericHook.prototype.unhook = function (node, propName, next) {
    if (GenericHook.canTransition(this, next)) {
        // Do nothing, the impending [hook] will handle the call to update.
    } else {
        var state = GenericHook.read_state(node, propName);
        this.destroy(state, node);
        GenericHook.remove_state(node, propName);
    }
};

joo_global_object.GenericHook = GenericHook;
