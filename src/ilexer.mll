{
  open Lexing
  open Iparser

  exception SyntaxError of string

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                 pos_lnum = pos.pos_lnum + 1
      }
}

let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let int = '-'? digit+

let frac = '.' digit*
let expn = ['e' 'E'] ['-' '+']? digit+
let float = digit* frac? expn?

let id = (alpha|'_') (alpha|digit|'_')*

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"

rule read = parse
  | int        { INT (int_of_string (Lexing.lexeme lexbuf)) }
  | float      { FLOAT (float_of_string (Lexing.lexeme lexbuf)) }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | "+"        { PLUS }
  | "-"        { MINUS }
  | "*"        { MULT } 
  | "/"        { DIV }
  | "%"        { MOD }
  | ">"        { GT }
  | ">="       { GE }
  | "<"        { LT }
  | "<="       { LE }
  | "="        { EQ }
  | "!="       { NEQ }
  | "&&"       { AND }
  | "||"       { OR }
  | "!"        { NOT }
  | id         { ID (Lexing.lexeme lexbuf) }
  | '"'        { read_string (Buffer.create 17) lexbuf }
  | whitespace { read lexbuf }
  | newline    { next_line lexbuf; read lexbuf }
  | eof        { EOF }
  | _          { raise (SyntaxError ("ILexer - Illegal character: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
  | '"'        { STRING (Buffer.contents buf) }
  | '\\' '/'   { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'  { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'   { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'   { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'   { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'   { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'   { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
                { Buffer.add_string buf (Lexing.lexeme lexbuf);
                  read_string buf lexbuf
                }
  | _           { raise (SyntaxError ("ILexer - Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof         { raise (SyntaxError ("ILexer - String is not terminated")) }
