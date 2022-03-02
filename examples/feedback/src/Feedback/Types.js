exports.onElts = function (v) {
	return function (i) {
		return v[i];
	};
};
exports.updateAtElt = function (f) {
	return function (v) {
		return function (i) {
			var o = v.slice();
      o[i] = f(o[i]);
      return o;
		};
	};
};
