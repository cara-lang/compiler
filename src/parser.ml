include Nice_parser.Make(struct
  type result = Syntax.stmt_list
  type token = Menhir_parser.token
  exception ParseError = Menhir_parser.Error
  let parse = Menhir_parser.main
  include Lexer
end)