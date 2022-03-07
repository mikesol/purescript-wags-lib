/**
 * Monkey patch `component` and `mkComponent` functions from `Halogen.Component`
 * module, so that component state persists across hot module reloading.
 */

import Component from "../output/Halogen.Component/index.js";

export * from "../output/Halogen.Component/index.js";

const cache = {};

const getState = (id) => {
	return cache[id];
};

const setState = (id, state) => {
	cache[id] = state;
};

const REGEX = /.*\((.*)\?.*/;

/**
 * stack trace looks like following, parse module file name from it.
 *
 * Error
 *   at getCallerFile (Halogen.Component.patch.js:29)
 *   at Module.component (Halogen.Component.patch.js:97)
 *   at eval (index.js:47)
 *   at eval (index.js:55)
 *   at Object../output/Example.Input/index.js (index.js:3866)
 *   at __webpack_require__ (index.js:712)
 *   at fn (index.js:95)
 *   at eval (index.js:9)
 *   at Object../output/Main/index.js (index.js:4394)
 *   at __webpack_require__ (index.js:712)
 */
function getCallerFile() {
	try {
		throw new Error();
	} catch (e) {
		const traces = e.stack.split("\n");
		const curFilename = traces[1].match(REGEX)[1];
		for (let i = 2; i < e.stack.length; i++) {
			const filename = traces[i].match(REGEX)[1];
			if (filename !== curFilename) {
				return filename;
			}
		}
	}
}

export const mkComponent = (config) => {
	const cacheId = getCallerFile();

	const initialState_ = config.initialState;
	const render_ = config.render;

	config.initialState = (...rest) => {
		return getState(cacheId) || initialState_(...rest);
	};

	config.render = (state) => {
		setState(cacheId, state);
		return render_(state);
	};
	return Component.mkComponent(config);
};
