
%token <Token_dts.number_type> T_NUMBER
%token <string> T_STRING
%token <Ast.Expression.TemplateLiteral.Element.t> T_TEMPLATE_PART
%token T_IDENTIFIER
%token <string> T_REGEXP
(* Syntax *)
%token T_LCURLY
%token T_RCURLY
%token T_LPAREN
%token T_RPAREN
%token T_LBRACKET
%token T_RBRACKET
%token T_SEMICOLON
%token T_COMMA
%token T_PERIOD
%token T_ARROW
%token T_ELLIPSIS
(* Keywords *)
%token T_FUNCTION
%token T_IF
%token T_IN
%token T_INSTANCEOF
%token T_RETURN
%token T_SWITCH
%token T_THIS
%token T_THROW
%token T_TRY
%token T_VAR
%token T_WHILE
%token T_WITH
%token T_CONST
%token T_LET
%token T_NULL
%token T_FALSE
%token T_TRUE
%token T_BREAK
%token T_CASE
%token T_CATCH
%token T_CONTINUE
%token T_DEFAULT
%token T_DO
%token T_FINALLY
%token T_FOR
%token T_CLASS
%token T_EXTENDS
%token T_STATIC
%token T_ELSE
%token T_NEW
%token T_DELETE
%token T_TYPEOF
%token T_VOID
%token T_ENUM
%token T_EXPORT
%token T_IMPORT
%token T_SUPER
%token T_IMPLEMENTS
%token T_INTERFACE
%token T_PACKAGE
%token T_PRIVATE
%token T_PROTECTED
%token T_PUBLIC
%token T_YIELD
%token T_DEBUGGER
%token T_DECLARE
%token T_TYPE
%token T_MODULE
(* Operators *)
%token T_RSHIFT3_ASSIGN
%token T_RSHIFT_ASSIGN
%token T_LSHIFT_ASSIGN
%token T_BIT_XOR_ASSIGN
%token T_BIT_OR_ASSIGN
%token T_BIT_AND_ASSIGN
%token T_MOD_ASSIGN
%token T_DIV_ASSIGN
%token T_MULT_ASSIGN
%token T_MINUS_ASSIGN
%token T_PLUS_ASSIGN
%token T_ASSIGN
%token T_PLING
%token T_COLON
%token T_OR
%token T_AND
%token T_BIT_OR
%token T_BIT_XOR
%token T_BIT_AND
%token T_EQUAL
%token T_NOT_EQUAL
%token T_STRICT_EQUAL
%token T_STRICT_NOT_EQUAL
%token T_LESS_THAN_EQUAL
%token T_GREATER_THAN_EQUAL
%token T_LESS_THAN
%token T_GREATER_THAN
%token T_LSHIFT
%token T_RSHIFT
%token T_RSHIFT3
%token T_PLUS
%token T_MINUS
%token T_DIV
%token T_MULT
%token T_MOD
%token T_NOT
%token T_BIT_NOT
%token T_INCR
%token T_DECR
(* XHP Tokens *)
%token T_XHP_OPEN_TAG
%token T_XHP_CLOSE_TAG
%token T_XHP_GT
%token T_XHP_SLASH_GT
%token T_XHP_ATTR
%token T_XHP_TEXT
(* Extra tokens *)
%token T_VIRTUAL_SEMICOLON
%token T_ERROR
%token T_EOF
(* JSX *)
%token T_JSX_IDENTIFIER
%token <string>T_JSX_TEXT 

%start <Ast_dts.program> program_impl
%%

program_impl:
  | T_NULL { [] }
  | T_LCURLY { [] };
