var PubNub = require("pubnub");
var uuidv4 = require("uuid/v4");

exports.pubnub_ = function (listener) {
	return function () {
		var pn = new PubNub({
			publishKey: "pub-c-ce744358-2fba-4c34-ac63-a8c1b06a826e",
			subscribeKey: "sub-c-fb43997a-9871-11ec-82a0-02d5075437d9",
			uuid: uuidv4(),
		});
		pn.subscribe({
			channels: ["feedback_diffs"],
		});
		pn.addListener({
			message: function (msg) {
				listener(msg.message)();
			},
		});
	};
};

exports.publish_ = function (pubnub) {
	return function (message) {
		return function () {
			pubnub.publish({
				channel: "feedback_diffs",
				message: message,
			});
		};
	};
};
