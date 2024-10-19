const path = require("path");

module.exports = {
  entry: path.join(__dirname, "output/MainCouchdb/index.js" ),
  output: {
    library: "perspectives-couchdb",
    libraryTarget: "commonjs2",
    filename: "perspectives-couchdb.js",
    path: path.join(__dirname, "dist")
  },
  watch: false,
  mode: "development",
  target: "node",
  module: {
    rules: []
  },
  externals: {
    "perspectives-proxy": {
      commonjs: 'perspectives-proxy',
      commonjs2: 'perspectives-proxy',
      amd: 'perspectives-proxy',
      root: "perspectivesProxy"
    },
    "eventsource": 'commonjs eventsource'
  }
};
