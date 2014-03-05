/* File parser.mly */
%{
open ParseTree;;
open Lexer;; (* For the line number *)

(* We can handle some common errors with more detail here so thats what we do *)
let errMissSemi line = 
    (Printf.fprintf stderr "Syntax error: Missing semicolon\nLine number:\t%d\nUnable to complete execution\n" line); 
    flush stderr; 
    (exit 8)
;;
%}
%token <int> INT
%token <string> STRING
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL
%token SEMI
%token LCURLB RCURLB
%token IF ELSE
%token WHILE
%token INJECT
%token LESS GREATER LESSEQ GREATEREQ 
%token EQ NOTEQ
%token NOT OR AND
%token ADD
%token ASS
%token PROC
%token OPENSTREAM CLOSESTREAM
%token TRUE FALSE
%token INC DEC PLUSEQ MINUSEQ TIMESEQ DIVEQ
%left PLUS MINUS        /* lowest precedence */
%left TIMES DIV         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start main             
%type <ParseTree.pTree> main
%type <ParseTree.pTree> value
%type <ParseTree.pTree> exp
%type <ParseTree.pTree> processBody
%type <ParseTree.pTree> cond
%type <ParseTree.pTree> globalVars
%%

/* The entry point of the program */
main:
 | PROC LCURLB processBody RCURLB                { $3 }
 | globalVars PROC LCURLB processBody RCURLB    { Node2("mainExtGV", $1, $4, !lineno) }
;

/* A list of golbal varaiable declarations */
globalVars:
 | name ASS value SEMI               { Node2("assG", $1, $3, !lineno) }
 | name ASS value SEMI globalVars    { Node2("GvarExt", Node2("assG", $1, $3, !lineno), $5, !lineno) }
 | name ASS value                    { (errMissSemi (!lineno-1)) }
 | name ASS value globalVars         { (errMissSemi (!lineno-2)) }
;

/* The process body which is what actually runs */
processBody:
   exp processBody                 { Node2("bodyExt", $1, $2, !lineno) }
 | exp                             { Node1("bodyEnd", $1, !lineno) }
;

/* Conditional statements (Note they must always have braces) */
cond:
   IF LPAREN value RPAREN LCURLB processBody RCURLB                                  { Node2("if", $3, $6, !lineno) }
 | IF LPAREN value RPAREN LCURLB processBody RCURLB ELSE LCURLB processBody RCURLB   { Node3("if", $3, $6, $10, !lineno) }
 | IF LPAREN value RPAREN LCURLB processBody RCURLB elif                             { Node3("if", $3, $6, $8, !lineno) }
;

elif:
 | ELSE IF LPAREN value RPAREN LCURLB processBody RCURLB elif   { Node3("if", $4, $7, $9, !lineno) }
 | ELSE IF LPAREN value RPAREN LCURLB processBody RCURLB        { Node3("if", $4, $7, Leaf(0, !lineno), !lineno) }
 | ELSE LCURLB processBody RCURLB                               { $3 }
;

loop:
   WHILE LPAREN value RPAREN LCURLB processBody RCURLB  { Node2("while", $3, $6, !lineno) }
;

/* The inject statment which actually adds elements to the output stream */
inj:
 | INJECT LPAREN value RPAREN SEMI       { Node1("inj", $3, !lineno) } 
 | INJECT LPAREN name RPAREN SEMI        { Node1("injName", $3, !lineno) }
 | INJECT LPAREN value RPAREN            { (errMissSemi (!lineno-1)) }
 | INJECT LPAREN name RPAREN             { (errMissSemi (!lineno-1)) }
;

/* One line expressions (with their semi-colons if needed) */
exp:
   value SEMI                { $1 }
 | cond                      { $1 }
 | loop                      { $1 }
 | inj                       { $1 }
 | name ASS value SEMI       { Node2("ass", $1, $3, !lineno) }
 | name PLUSEQ value SEMI    { Node2("+=", $1, $3, !lineno) }
 | name MINUSEQ value SEMI   { Node2("-=", $1, $3, !lineno) }
 | name TIMESEQ value SEMI   { Node2("*=", $1, $3, !lineno) }
 | name DIVEQ value SEMI     { Node2("/=", $1, $3, !lineno) }
 | name ASS value            { (errMissSemi (!lineno-1)) }
 | name DIVEQ value          { (errMissSemi (!lineno-1)) }
 | name PLUSEQ value         { (errMissSemi (!lineno-1)) }
 | name MINUSEQ value        { (errMissSemi (!lineno-1)) }
 | name TIMESEQ value        { (errMissSemi (!lineno-1)) }
 | name DIVEQ value          { (errMissSemi (!lineno-1)) }
;

/* Variable names */
name:
 |  STRING                       { Name( $1 , !lineno) }
 |  OPENSTREAM value CLOSESTREAM { Node1("streamV", $2, !lineno) }
;

/* values (without semi-colons so they can be part of other values) */
value:
   INT                         { Leaf($1, !lineno) }
 | LPAREN value RPAREN         { Node1("value", $2, !lineno) }
 | value PLUS value            { Node2("+", $1, $3, !lineno) }
 | value MINUS value           { Node2("-", $1, $3, !lineno) }
 | value TIMES value           { Node2("*", $1, $3, !lineno) }
 | value DIV value             { Node2("/", $1, $3, !lineno) }
 | MINUS value %prec UMINUS    { Node1("-", $2, !lineno) }
 | TRUE                        { Leaf(1, !lineno) }
 | FALSE                       { Leaf(0, !lineno) }
 | name                        { $1 }
 | value LESS value            { Node2("<", $1, $3, !lineno) }
 | value GREATER value         { Node2(">", $1, $3, !lineno) }
 | value LESSEQ value          { Node2("<=", $1, $3, !lineno) }
 | value GREATEREQ value       { Node2(">=", $1, $3, !lineno) }
 | value EQ value              { Node2("==", $1, $3, !lineno) }
 | value NOTEQ value           { Node2("!=", $1, $3, !lineno) }
 | value OR value              { Node2("or", $1, $3, !lineno) }
 | value AND value             { Node2("and", $1, $3, !lineno) }
 | NOT value                   { Node1("NOT", $2, !lineno) }
 | name INC                    { Node1("++", $1, !lineno) }
 | name DEC                    { Node1("--", $1, !lineno) }
;
