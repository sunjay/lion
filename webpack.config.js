module.exports = {
  entry: [
    "babel-polyfill",
    "./index.js"
  ],
  context: __dirname + "/src",
  output: {
    library: "LION",
    filename: 'lion.js',
    path: './lib'
  },
  module: {
    loaders: [
      { test: /\.js$/, exclude: /node_modules/, loader: "babel-loader"}
    ]
  }
};
