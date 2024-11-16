%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token EQUAL_EQUAL
%token EQUAL
%token BANG_EQUAL
%token BANG
%token COMMA
%token SEMICOLON
%token DOT
%token PLUS
%token MINUS
%token STAR
%token SLASH
%token EOF
%token <Char.t> UNKNOWN
%token <String.t> IDENTIFIER
%start <Unit.t> gobble

%%

gobble:
    | noteof* EOF { () }
noteof: LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE | COMMA | SEMICOLON | DOT | PLUS | MINUS | STAR | SLASH | UNKNOWN | EQUAL_EQUAL | EQUAL | BANG_EQUAL | BANG { () }