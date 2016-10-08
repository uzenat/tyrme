{
open Parser        (* The type token is defined in parser.mli *)

exception Eof
}

rule token = parse
       | [' ' '\t' '\n']   { token lexbuf }     (* skip blanks *)
       | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
       | '+'            { PLUS }
       | '-'            { SUBS }
       | '*'            { MULT }
       | '/'            { DIV }
       | '='            { EQ }
       | "=="           { EQEQ }
       | "<="           { LEQ }
       | "and"          { AND }
       | "if"           { IF }
       | "then"         { THEN_ }
       | "else"         { ELSE }
       | '^'            { CAT }
       | "fst"          { FST }
       | "snd"          { SND }
       | "proj"         { PROJ }
       | "let"          { LET }
       | "in"           { IN }
       | "print"        { PRINT }
       | "()"           { UNIT }
       | '('            { LPAREN }
       | ')'            { RPAREN }
       | "[|"            { LCROCH }
       | "|]"            { RCROCH }
       | ';'            { SEMICOLON }
       | ','            { COMMA }
       | "true"         { BOOL(true) }
       | "false"        { BOOL(false) }
       | "for"          { FOR }
       | "to"           { TO }
       | "do"           { DO }
       | "done"         { DONE }
       | ['a'-'z' '_']+ as v  { VAR(v) }
       | eof            { EOL }
       | '"'            { let buffer = Buffer.create 20 in
                          STRING (stringl buffer lexbuf)
                        }
 and  stringl buffer = parse
       | '"'       { Buffer.contents buffer }
       | "\\t"     { Buffer.add_char buffer '\t'; stringl buffer lexbuf }
       | "\\n"     { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
       | "\\n"     { Buffer.add_char buffer '\n'; stringl buffer lexbuf }
       | '\\' '"'  { Buffer.add_char buffer '"';  stringl buffer lexbuf }
       | '\\' '\\' { Buffer.add_char buffer '\\'; stringl buffer lexbuf }
       | eof       { raise End_of_file }
       | _ as char { Buffer.add_char buffer char; stringl buffer lexbuf }
