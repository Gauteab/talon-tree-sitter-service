exports.gotoFirstChild = (c) => () => c.gotoFirstChild();
exports.gotoFirstChildForIndex = (i) => (c) => () =>
  c.gotoFirstChildForIndex(i);
exports.gotoNextSibling = (c) => () => c.gotoNextSibling();
exports.gotoParent = (c) => () => c.gotoParent();
exports.reset = (c) => c.reset();
