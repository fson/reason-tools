const path = require('path');
const webpack = require('webpack');
const GenerateJsonPlugin = require('generate-json-webpack-plugin');
const manifest = require('./src/manifest.json');
const package = require('./package.json');

manifest.version = package.version;

module.exports = {
  entry: {
    AutoConversionPrompt: './lib/js/src/extension/autoConversionPrompt.js',
    Popup: './lib/js/src/extension/popup.js',
    Background: './lib/js/src/extension/background.js',
  },
  output: {
    path: path.join(__dirname, '_build/extension/'),
    filename: '[name].bundle.js',
  },
  module: {
    loaders: [
      {
        test: /\.js$/,
        exclude: /(node_modules|_build)/,
        loader: 'babel-loader',
        query: {
          presets: ['latest', 'react', 'stage-0']
        }
      },
      {
        test: /\.(png|jpg|gif|html|css)$/,
        loader: 'file-loader?name=[name].[ext]'
      }
    ],
  },
  plugins: [
    new GenerateJsonPlugin('manifest.json', manifest),
  ],
};
