const { Query } = require("tree-sitter");

exports.new = (language) => (source) => () => new Query(language, source);
exports.captures = (query) => (node) => () => query.captures(node);
exports.matches = (query) => (node) => () => query.matches(node);
