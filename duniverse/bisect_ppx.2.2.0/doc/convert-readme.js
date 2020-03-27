const gfm = require('cmark-gfm-js');
const fs = require('fs');

const markdown = fs.readFileSync(0, 'utf-8');
const html = gfm.convert(markdown);
console.log(html);
