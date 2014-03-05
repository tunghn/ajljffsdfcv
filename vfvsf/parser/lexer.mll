(* File lexer.mll *)
{
open Parser        (* The type token is defined in parser.mli *)
exception Eof
let lineno = ref 1;;
}
rule token = parse
      [' ''\t']     { token lexbuf }     (* skip blanks *)
    | ['\n']        {lineno := (!lineno +1); token lexbuf }
    | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
    | "|*" [^'|''*']* "*|" {token lexbuf}
    | '+'       { PLUS }
    | '-'       { MINUS }
    | '*'       { TIMES }
    | '/'       { DIV }
    | '('       { LPAREN }   
    | ')'       { RPAREN }
    | ';'       { SEMI }
    | '{'       { LCURLB }
    | '}'       { RCURLB }
    | "if"      { IF }
    | "else"    { ELSE }
    | "while"   { WHILE }
    | "inject"  { INJECT }
    | '<'       { LESS }
    | "<="      { LESSEQ }
    | "||"      { OR }
    | "&&"      { AND }
    | '>'       { GREATER }
    | ">="      { GREATEREQ }
    | "=="      { EQ }
    | "!="      { NOTEQ }
    | '!'       { NOT }
    | '='       { ASS }
    | "process" { PROC }
    | "TRUE"    { TRUE }
    | "FALSE"   { FALSE }
    | "stream[" { OPENSTREAM }
    | ']'       { CLOSESTREAM }
    | "++"      { INC }
    | "--"      { DEC }
    | "+="      { PLUSEQ }
    | "-="      { MINUSEQ }
    | "*="      { TIMESEQ }
    | "/="      { DIVEQ }
    | ['a'-'z''A'-'Z''_']['a'-'z''A'-'Z''_''0'-'9']* as t { STRING( t ) }
    | eof      { raise Eof }
