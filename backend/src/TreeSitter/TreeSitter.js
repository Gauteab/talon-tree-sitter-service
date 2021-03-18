const Parser = require("tree-sitter");
const Elm = require("tree-sitter-elm");

exports.elmLanguage = Elm;

exports.toString = (x) => x.toString();
exports.json = (x) => JSON.stringify(x);
