const Parser = require("tree-sitter");

exports.new = (language) => () => {
  const parser = new Parser();
  parser.setLanguage(language);
  return parser;
};

exports.parse = (s) => (p) => () => p.parse(s);
