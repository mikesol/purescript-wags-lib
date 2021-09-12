const tf = require('@tensorflow/tfjs');
const HMM = require('hmm');

exports.getObservations_ = function (options) {
  return async function () {
    // observations must be 4 or larger, otherwise
    // it complains that the matrix isn't invertible
    const [observations, time, states, dimensions] = [options.observations, options.time, options.states, options.dimensions];

    // Configure model
    const hmm = new HMM({
      states: states,
      dimensions: dimensions
    });
    // Set parameters
    await hmm.setParameters({
      pi: tf.tensor(options.pi),
      A: tf.tensor(options.A),
      mu: tf.tensor(options.mu),
      Sigma: tf.tensor(options.Sigma)
    });

    // Sample data
    const sample = hmm.sample({ observations, time });
    //assert.deepEqual(sample.states.shape, [observations, time]);
    //assert.deepEqual(sample.emissions.shape, [observations, time, dimensions]);

    // Your data must be a tf.tensor with shape [observations, time, dimensions]
    const data = sample.emissions;
    // Fit model with data
    const results = await hmm.fit(data);

    // Predict hidden state indices
    const inference = hmm.inference(data);
    const res = Array.from(inference.dataSync());
    var out = [];
    for (var i = 0; i < observations; i++) {
      var temp = [];
      for (var j = 0; j < time; j++) {
        temp.push(res[i * observations + j]);
      }
      out.push(temp)
    }
    return out;
  }
}

