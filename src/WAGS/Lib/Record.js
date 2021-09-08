var applyRecordImpl = function (fns) {
  return function (a) {
    var b = {}
    for (let key in fns) {
      if (fns.hasOwnProperty(key)) {
        b[key] = typeof fns[key] === 'function' ? fns[key](a[key]) : applyRecordImpl(fns[key])(a[key]);
      }
    }
    return b;
  }
}
exports.applyRecordImpl = applyRecordImpl;