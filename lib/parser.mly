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
%start <Ast.expr> expression
%start <Ast.program> prog

%%

let triple(a, b, c) ==
    ~ = a; ~ = b; ~ = c; { (a, b, c) }

let gobble :=
    | gobble_not_eof* ; EOF ; { () }

let gobble_not_eof :=
    | LEFT_PAREN | RIGHT_PAREN | LEFT_BRACE | RIGHT_BRACE
    | COMMA | DOT | MINUS | PLUS | SEMICOLON | SLASH | STAR
    
    | BANG | BANG_EQUAL
    | EQUAL | EQUAL_EQUAL
    | GREATER | GREATER_EQUAL
    | LESS | LESS_EQUAL

    | IDENTIFIER; { () } | STRING; { () } | NUMBER; { () }

    | AND | CLASS | ELSE | FALSE | FUN | FOR | IF | NIL | OR
    | PRINT | RETURN | SUPER | THIS | TRUE | VAR | WHILE

let expression := terminated(expr, EOF)

let expr := assignment

let assignment :=
    | ~ = separated_pair(IDENTIFIER, EQUAL, expr); < Ast.Assign >
    | logic_or

let logic_or :=
    | ~ = triple(logic_and, orop, logic_or); SEMICOLON; < Ast.Logical >
    | logic_and

let orop :=
    | OR; { Ast.Or }

let logic_and :=
    | ~ = triple(equality, andop, logic_and); SEMICOLON; < Ast.Logical >
    | equality

let andop :=
    | AND; { Ast.And }

let equality :=
    | ~ = triple(equality, eqop, comparison); < Ast.Binary >
    | comparison

let eqop :=
    | BANG_EQUAL; { Ast.Bang_equal }
    | EQUAL_EQUAL; { Ast.Equal_equal }

let comparison :=
    | ~ = triple(comparison, compop, term); < Ast.Binary >
    | term

let compop :=
    | GREATER; { Ast.Greater }
    | GREATER_EQUAL; { Ast.Greater_equal }
    | LESS; { Ast.Less }
    | LESS_EQUAL; { Ast.Less_equal }

let term :=
    | ~ = triple(term, term_binop, factor); < Ast.Binary >
    | factor

let term_binop :=
    | MINUS; { Ast.Minus }
    | PLUS; { Ast.Plus }

let factor :=
    | ~ = triple(factor, factor_binop, unary); < Ast.Binary >
    | unary

let factor_binop :=
    | SLASH; { Ast.Slash }
    | STAR; { Ast.Star }

let unary :=
    | ~ = pair(unop, unary); < Ast.Unary >
    | primary

let unop :=
    | MINUS; { Ast.Minus }
    | BANG; { Ast.Bang }

let primary :=
    | ~ = literal; < Ast.Literal >
    // | THIS; { Ast.This "this" }
    | identifier
    | delimited(LEFT_PAREN, grouping, RIGHT_PAREN)

let literal :=
    | ~ = NUMBER; < Ast.Number >
    | ~ = STRING; < Ast.String >
    | TRUE; { Ast.Boolean true }
    | FALSE; { Ast.Boolean false }
    | NIL; { Ast.Nil }

let identifier := ~ = IDENTIFIER; < Ast.Variable >

let grouping := ~ = expr; < Ast.Grouping >

let prog := ~ = declaration*; EOF; <>

let declaration :=
    | vardecl
    | statement

let vardecl := ~ = delimited(VAR, pair(IDENTIFIER, option(preceded(EQUAL, expr))), SEMICOLON); < Ast.Var >

let statement :=
    | ~ = delimited(PRINT, expr, SEMICOLON); < Ast.Print >
    | ~ = terminated(expr, SEMICOLON); < Ast.Expression >
    | ~ = delimited(LEFT_BRACE, declaration*, RIGHT_BRACE); < Ast.Block >
