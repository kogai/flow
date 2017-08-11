(**
 * Copyright (c) 2013-present, Facebook, Inc.
 * All rights reserved.
 *
 * This source code is licensed under the BSD-style license found in the
 * LICENSE file in the "flow" directory of this source tree. An additional grant
 * of patent rights can be found in the PATENTS file in the same directory.
 *
 *)

module Ast = Dts_ast

type token =
  | T_NUMBER of number_type
  | T_STRING of (Loc.t * string * string * bool) (* loc, value, raw, octal *)
  | T_TEMPLATE_PART of Ast.Expression.TemplateLiteral.Element.t
  | T_IDENTIFIER
  | T_REGEXP of (Loc.t * string * string) (* /pattern/flags *)
  (* Syntax *)
  | T_LCURLY
  | T_RCURLY
  | T_LPAREN
  | T_RPAREN
  | T_LBRACKET
  | T_RBRACKET
  | T_SEMICOLON
  | T_COMMA
  | T_PERIOD
  | T_ARROW
  | T_ELLIPSIS
  (* Keywords *)
  | T_FUNCTION
  | T_IF
  | T_IN
  | T_INSTANCEOF
  | T_RETURN
  | T_SWITCH
  | T_THIS
  | T_THROW
  | T_TRY
  | T_VAR
  | T_WHILE
  | T_WITH
  | T_CONST
  | T_LET
  | T_NULL
  | T_FALSE
  | T_TRUE
  | T_BREAK
  | T_CASE
  | T_CATCH
  | T_CONTINUE
  | T_DEFAULT
  | T_DO
  | T_FINALLY
  | T_FOR
  | T_CLASS
  | T_EXTENDS
  | T_STATIC
  | T_ELSE
  | T_NEW
  | T_DELETE
  | T_TYPEOF
  | T_VOID
  | T_ENUM
  | T_EXPORT
  | T_IMPORT
  | T_SUPER
  | T_IMPLEMENTS
  | T_INTERFACE
  | T_PACKAGE
  | T_PRIVATE
  | T_PROTECTED
  | T_PUBLIC
  | T_YIELD
  | T_DEBUGGER
  | T_DECLARE
  | T_TYPE
  | T_MODULE
  (* Operators *)
  | T_RSHIFT3_ASSIGN
  | T_RSHIFT_ASSIGN
  | T_LSHIFT_ASSIGN
  | T_BIT_XOR_ASSIGN
  | T_BIT_OR_ASSIGN
  | T_BIT_AND_ASSIGN
  | T_MOD_ASSIGN
  | T_DIV_ASSIGN
  | T_MULT_ASSIGN
  | T_MINUS_ASSIGN
  | T_PLUS_ASSIGN
  | T_ASSIGN
  | T_PLING
  | T_COLON
  | T_OR
  | T_AND
  | T_BIT_OR
  | T_BIT_XOR
  | T_BIT_AND
  | T_EQUAL
  | T_NOT_EQUAL
  | T_STRICT_EQUAL
  | T_STRICT_NOT_EQUAL
  | T_LESS_THAN_EQUAL
  | T_GREATER_THAN_EQUAL
  | T_LESS_THAN
  | T_GREATER_THAN
  | T_LSHIFT
  | T_RSHIFT
  | T_RSHIFT3
  | T_PLUS
  | T_MINUS
  | T_DIV
  | T_MULT
  | T_MOD
  | T_NOT
  | T_BIT_NOT
  | T_INCR
  | T_DECR
  (* XHP Tokens *)
  | T_XHP_OPEN_TAG
  | T_XHP_CLOSE_TAG
  | T_XHP_GT
  | T_XHP_SLASH_GT
  | T_XHP_ATTR
  | T_XHP_TEXT
  (* Extra tokens *)
  | T_VIRTUAL_SEMICOLON
  | T_ERROR
  | T_EOF
  (* JSX *)
  | T_JSX_IDENTIFIER
  | T_JSX_TEXT of (Loc.t * string * string) (* loc, value, raw *)

and number_type =
  | OCTAL
  | NORMAL


(*****************************************************************************)
(* Pretty printer (pretty?) *)
(*****************************************************************************)
let token_to_string = function
| T_NUMBER _ -> "NUMBER"
| T_STRING _ -> "STRING"
| T_TEMPLATE_PART _ -> "TEMPLATE PART"
| T_IDENTIFIER -> "IDENTIFIER"
| T_REGEXP _ -> "REGEXP"
| T_FUNCTION -> "function"
| T_IF -> "if"
| T_IN -> "in"
| T_INSTANCEOF -> "instanceof"
| T_RETURN -> "return"
| T_SWITCH -> "switch"
| T_THIS -> "this"
| T_THROW -> "throw"
| T_TRY -> "try"
| T_VAR -> "var"
| T_WHILE -> "while"
| T_WITH -> "with"
| T_CONST -> "const"
| T_LET  -> "let"
| T_NULL -> "null"
| T_FALSE -> "false"
| T_TRUE -> "true"
| T_BREAK -> "break"
| T_CASE -> "case"
| T_CATCH -> "catch"
| T_CONTINUE -> "continue"
| T_DEFAULT -> "default"
| T_DO -> "do"
| T_FINALLY -> "finally"
| T_FOR -> "for"
| T_CLASS -> "class"
| T_EXTENDS -> "extends"
| T_STATIC -> "static"
| T_ELSE -> "else"
| T_NEW -> "new"
| T_DELETE -> "delete"
| T_TYPEOF -> "typeof"
| T_VOID -> "void"
| T_ENUM -> "enum"
| T_EXPORT  -> "export"
| T_IMPORT -> "import"
| T_SUPER  -> "super"
| T_IMPLEMENTS -> "implements"
| T_INTERFACE -> "interface"
| T_PACKAGE -> "package"
| T_PRIVATE -> "private"
| T_PROTECTED -> "protected"
| T_PUBLIC -> "public"
| T_YIELD -> "yield"
| T_DEBUGGER -> "debugger"
| T_DECLARE -> "declare"
| T_TYPE -> "type"
| T_MODULE -> "module"
| T_LCURLY -> "{"
| T_RCURLY -> "}"
| T_LPAREN -> "("
| T_RPAREN -> ")"
| T_LBRACKET -> "["
| T_RBRACKET -> "]"
| T_SEMICOLON -> ";"
| T_COMMA -> ","
| T_PERIOD -> "."
| T_ARROW -> "=>"
| T_ELLIPSIS -> "..."
| T_RSHIFT3_ASSIGN -> ">>>="
| T_RSHIFT_ASSIGN -> ">>="
| T_LSHIFT_ASSIGN -> "<<="
| T_BIT_XOR_ASSIGN -> "^="
| T_BIT_OR_ASSIGN -> "|="
| T_BIT_AND_ASSIGN -> "&="
| T_MOD_ASSIGN -> "%="
| T_DIV_ASSIGN -> "/="
| T_MULT_ASSIGN -> "*="
| T_MINUS_ASSIGN -> "-="
| T_PLUS_ASSIGN -> "+="
| T_ASSIGN -> "="
| T_PLING -> "?"
| T_COLON -> ":"
| T_OR -> "||"
| T_AND -> "&&"
| T_BIT_OR -> "|"
| T_BIT_XOR -> "^"
| T_BIT_AND -> "&"
| T_EQUAL -> "=="
| T_NOT_EQUAL -> "!="
| T_STRICT_EQUAL -> "==="
| T_STRICT_NOT_EQUAL -> "!=="
| T_LESS_THAN_EQUAL -> "<="
| T_GREATER_THAN_EQUAL -> ">="
| T_LESS_THAN -> "<"
| T_GREATER_THAN -> ">"
| T_LSHIFT -> "<<"
| T_RSHIFT -> ">>"
| T_RSHIFT3 -> ">>>"
| T_PLUS -> "+"
| T_MINUS -> "-"
| T_DIV -> "/"
| T_MULT -> "*"
| T_MOD -> "%"
| T_NOT -> "!"
| T_BIT_NOT -> "~"
| T_INCR -> "++"
| T_DECR -> "--"
(* XHP Tokens *)
| T_XHP_OPEN_TAG -> "<XHP"
| T_XHP_CLOSE_TAG -> "</XHP>"
| T_XHP_GT -> ">"
| T_XHP_SLASH_GT -> "/>"
| T_XHP_ATTR -> "XHP_ATTR"
| T_XHP_TEXT -> "XHP_TEXT"
(* Extra tokens *)
| T_ERROR -> "ERROR"
| T_VIRTUAL_SEMICOLON -> ";"
| T_EOF -> "EOF"
| T_JSX_IDENTIFIER -> "JSX IDENTIFIER"
| T_JSX_TEXT _ -> "JSX TEXT"
