/* JavaScript code for the main process            -*- js-indent-level: 2 -*- */

'use strict';

const electron = require('electron');
const app = electron.app;
const readline = require('readline');

let windows = {};

readline.createInterface({
  input: process.stdin,
  terminal: false
}).on('line', eval);

app.once('will-finish-launching', function() {
  const event = { emitter: 'app', event: 'will-finish-launching' };
  process.stdout.write('[CERAMIC] ' + JSON.stringify(event) + '\n');
});
