function VdomThunk(fn, args, key) {
    if (!(this instanceof VdomThunk)) {
        return new VdomThunk(fn, args, key);
    }

    if (key) {
      this.key = key;
    }

    this.fn = fn;
    this.args = args;
};

joo_global_object.VdomThunk = VdomThunk;

VdomThunk.prototype.type = 'Thunk';
VdomThunk.prototype.render = function (prev) {
  if (prev && this.args === prev.args && this.fn === prev.fn) {
    return prev.vnode;
  }

  return this.fn(this.args);
};


