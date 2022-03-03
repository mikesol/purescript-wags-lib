exports.normalizedWidthAndHeight_ = function (tpl) {
	return function (evt) {
		var e = evt.target;
		var dim = e.getBoundingClientRect();
		var x = evt.clientX - dim.left;
		var y = evt.clientY - dim.top;
		var nx = x / dim.width;
		var ny = y / dim.height;
		return tpl(nx)(ny);
	};
};
