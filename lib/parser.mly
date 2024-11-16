%token LEFT_PAREN
%token RIGHT_PAREN
%token LEFT_BRACE
%token RIGHT_BRACE
%token COMMA
%token DOT
%token MINUS
%token PLUS
%token SEMICOLON
%token SLASH
%token STAR

%token BANG
%token BANG_EQUAL
%token EQUAL
%token EQUAL_EQUAL
%token GREATER
%token GREATER_EQUAL
%token LESS
%token LESS_EQUAL

%token <String.t> IDENTIFIER
%token <String.t> STRING
%token <Float.t> NUMBER

%token AND
%token CLASS
%token ELSE
%token FALSE
%token FUN
%token FOR
%token IF
%token NIL
%token OR
%token PRINT
%token RETURN
%token SUPER
%token THIS
%token TRUE
%token VAR
%token WHILE

%token EOF
%token <String.t> UNKNOWN

%start <Unit.t> gobble

%%

gobble:
    | noteof* EOF { () }
noteof:
    | LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE
    | COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR
    
    | BANG | BANG_EQUAL
    | EQUAL | EQUAL_EQUAL
    | GREATER | GREATER_EQUAL
    | LESS | LESS_EQUAL

    | IDENTIFIER | STRING | NUMBER

    | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR
    | PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE
    { () }