var PubNub = require("pubnub");
var uuidv4 = require("uuid").v4;

exports.pubnub_ = function (listener) {
	return function () {
		var publisher = uuidv4();
		var pn = new PubNub({
			publishKey: "pub-c-ce744358-2fba-4c34-ac63-a8c1b06a826e",
			subscribeKey: "sub-c-fb43997a-9871-11ec-82a0-02d5075437d9",
			uuid: publisher,
		});
		pn.subscribe({
			channels: ["feedback_diffs"],
		});
		pn.addListener({
			message: function (msg) {
				if (msg.publisher !== publisher) {
					listener(msg.message)();
				}
			},
		});
		return pn;
	};
};

exports.publish_ = function (pubnub) {
	return function (message) {
		console.log("Publishing message: " + JSON.stringify(message), "via pubnub ", pubnub);
		return function () {
			pubnub.publish({
				channel: "feedback_diffs",
				message: message,
			});
		};
	};
};
