function cloneAudioBuffer(context, audioBuffer) {
	var channels = [],
		numChannels = audioBuffer.numberOfChannels;

	//clone the underlying Float32Arrays
	for (var i = 0; i < numChannels; i++) {
		channels[i] = new Float32Array(audioBuffer.getChannelData(i));
	}

	//create the new AudioBuffer (assuming AudioContext variable is in scope)
	var newBuffer = context.createBuffer(
		audioBuffer.numberOfChannels,
		audioBuffer.length,
		audioBuffer.sampleRate
	);

	//copy the cloned arrays to the new AudioBuffer
	for (var i = 0; i < numChannels; i++) {
		newBuffer.getChannelData(i).set(channels[i]);
	}

	return newBuffer;
}

exports.reverseAudioBuffer = function (context) {
	return function (audioBuffer) {
		return function () {
			var cloned = cloneAudioBuffer(context, audioBuffer);
			for (var i = 0; i < cloned.numberOfChannels; i++) {
				Array.prototype.reverse.call(cloned.getChannelData(i));
			}
			return cloned;
		};
	};
};
exports.decodeAudioBufferFromArrayBuffer = function (ctx) {
	return function (b) {
		return function () {
			{
				return ctx.decodeAudioData(b);
			}
		};
	};
};
exports.fetchArrayBufferFromUri = function (s) {
	return function () {
		{
			return fetch(s).then(
				function (b) {
					return b.arrayBuffer();
				},
				function (e) {
					console.error('Error fetching buffer', e);
					return Promise.reject(e);
				}
			);
		}
	};
};
