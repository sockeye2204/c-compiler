open Extras
exception LexerError of string

type token_def = {
  re : Re.re;
  converter : string -> Token.t;
}

type match_def = {
  matched_substr : string;
  matching_token : token_def;
}

let literal tokn _str = tokn

let conv_iden = function
  | "int" -> Token.KWInt
  | "return" -> Token.KWReturn
  | "void" -> Token.KWVoid
  | other -> Token.Identifier other

let conv_int str = Token.Constant (int_of_string str)

let token_defs =
  let def re_str converter =
    {
      re = Re.Pcre.regexp ~flags: [`ANCHORED] re_str; converter
    }
  in
  [
    def {_|[a-zA-Z_][a-zA-Z0-9_]*\b|_} conv_iden;
    def {_|[0-9]+\b|_} conv_int;
    def {_|\(|_} (literal Token.ParenOpen);
    def {_|\)|_} (literal Token.ParenClose);
    def {_|\{|_} (literal Token.BraceOpen);
    def {_|\}|_} (literal Token.BraceClose);
    def ";" (literal Token.Semicolon);
    def "--" (literal Token.Decrement);
    def "-" (literal Token.Negation);
    def "~" (literal Token.BWComplement);
    def {_|\+|_} (literal Token.Addition);
    def {_|\*|_} (literal Token.Multiplication);
    def {_|/|_} (literal Token.Division);
    def {_|%|_} (literal Token.Remainder);
    def "!" (literal Token.LogicalNot);
    def "&&" (literal Token.LogicalAnd);
    def "\\|\\|" (literal Token.LogicalOr);
    def "==" (literal Token.EqualTo);
    def "!=" (literal Token.NotEqualTo);
    def "<" (literal Token.LessThan);
    def ">" (literal Token.GreaterThan);
    def "<=" (literal Token.LessThanOrEqualTo);
    def ">=" (literal Token.GreaterThanOrEqualTo);
    def "=" (literal Token.Assignment);
  ]

let find_match str token_def =
  let re = token_def.re in
  let pot_match = Re.exec_opt re str in
  match pot_match with
  | Some mat ->
    Some { matched_substr = Re.Group.get mat 0; matching_token = token_def}
  | None -> None

let leading_whitespace str =
  let whitespace_matcher = Re.Pcre.regexp ~flags: [`ANCHORED] {|\s+|} in
  let whitespace_match = Re.exec_opt whitespace_matcher str in
  match whitespace_match with
  | None -> None
  | Some mtch ->
    let _, match_end = Re.Group.offset mtch 0 in
    Some match_end

let skip_preprocessor str =
  let preproc = Re.Pcre.regexp ~flags:[`ANCHORED] {|#[^\n]*|} in
  match Re.exec_opt preproc str with
  | Some mtch ->
    let _, match_end = Re.Group.offset mtch 0 in
    Some match_end
  | None -> None

let skip_comment str =
  let single_line_comment = Re.Pcre.regexp ~flags: [`ANCHORED] {|//.*|} in
  let block_comment = Re.Pcre.regexp ~flags: [`ANCHORED] {|/\*.*?\*/|} in
  match Re.exec_opt single_line_comment str with
  | Some mtch ->
    let _, match_end = Re.Group.offset mtch 0 in
    Some match_end
  | None ->
    match Re.exec_opt block_comment str with
    | Some mtch ->
      let _, match_end = Re.Group.offset mtch 0 in
      Some match_end
    | None -> None

let rec lexer input =
  if input = "" then []
  else
    match leading_whitespace input with
    | Some whitespace_count -> lexer (StringUtil.drop whitespace_count input)
    | None ->
      match skip_comment input with
      | Some comment_count -> lexer (StringUtil.drop comment_count input)
      | None ->
        match skip_preprocessor input with
        | Some preproc_count -> lexer (StringUtil.drop preproc_count input)
        | None ->
          let matches = List.filter_map (find_match input) token_defs in
          if matches = [] then raise (LexerError input)
          else
            let compare_match_lengths m1 m2 =
              Int.compare
                (String.length m1.matched_substr)
                (String.length m2.matched_substr)
            in
            let longest_match = ListUtil.max compare_match_lengths matches in
            let converter = longest_match.matching_token.converter in
            let matching_substr = longest_match.matched_substr in
            let next_tok = converter matching_substr in
            let remaining =
              StringUtil.drop
                (String.length longest_match.matched_substr)
                input
            in
            next_tok :: lexer remaining
