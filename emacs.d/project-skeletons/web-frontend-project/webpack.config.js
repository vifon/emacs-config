var path = require('path');

module.exports = {
  entry: './app/script.js',
  output: {
    filename: 'bundle.js',
    path: path.resolve(__dirname, 'dist/js/')
  }
};
