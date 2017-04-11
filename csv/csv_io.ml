type error_location = {
  e_line_number : int;
  e_start : int;
  e_end : int
}

let error_location lexbuf =
  let open Lexing in
  let pos1 = lexbuf.lex_start_p in
  let pos2 = lexbuf.lex_curr_p in
  let line1 = pos1.pos_lnum
  and start1 = pos1.pos_bol in
  {
    e_line_number = line1;
    e_start = pos1.pos_cnum - start1;
    e_end = pos2.pos_cnum - start1
  }

let string_of_error_location {e_line_number; e_start; e_end} =
  Printf.sprintf "error at line %d, characters %d-%d\n" e_line_number
    e_start e_end

type error = [
  | `SyntaxError of error_location
  | `UnterminatedString of int (* line number *)
  | `IntOverflow of (int * string) (* line number and offending string *)
]

type next_row = unit -> [ `Ok of Csv_types.row | error ]

let header_of_string s =
  let lexbuf = Lexing.from_string s in
  Csv_parser.header Csv_lexer.header lexbuf

let of_channel ~no_header ch =
  let lexbuf = Lexing.from_channel ch in
  try
    let h =
      if not no_header then
        Csv_parser.header Csv_lexer.header lexbuf
      else
        []
    in
    let row () =
      try
        `Ok (Csv_parser.row Csv_lexer.row lexbuf)
      with
        | Parsing.Parse_error ->
          `SyntaxError (error_location lexbuf)
        | Csv_lexer.UnterminatedString line ->
          `UnterminatedString line
        | Csv_lexer.IntOverflow line_and_offending_string ->
          `IntOverflow line_and_offending_string
    in
    `Ok (h, row)

  with
    | Parsing.Parse_error ->
      `SyntaxError (error_location lexbuf)
    | Csv_lexer.UnterminatedString line ->
      `UnterminatedString line
    | Csv_lexer.IntOverflow line_and_offending_string ->
      `IntOverflow line_and_offending_string
