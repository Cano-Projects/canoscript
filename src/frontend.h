#pragma once
#ifndef FRONTEND_H
#define FRONTEND_H

#include "defs.h"

// Lexing
    
bool is_valid_escape(char c);
String_View read_file_to_view(Arena *arena, char *filename);
bool isword(char c);
Token_Type get_token_type(String_View str);
Token handle_data_type(Token token, String_View str);
bool is_operator(String_View view);
Token create_operator_token(char *filename, size_t row, size_t col, String_View *view);
void print_token_arr(Token_Arr arr);
Token_Arr lex(Arena *arena, Arena *string_arena, char *filename, String_View view);
Token token_consume(Token_Arr *tokens);
Token token_peek(Token_Arr *tokens, size_t peek_by);
Token expect_token(Token_Arr *tokens, Token_Type type);
Node *create_node(Arena *arena, Node_Type type);
Precedence op_get_prec(Token_Type type);
Operator create_operator(Token_Type type);
Expr *parse_expr(Parser *parser);
Expr *parse_primary(Parser *parser);
Expr *parse_expr_1(Parser *parser, Expr *lhs, Precedence min_precedence);
Node parse_native_node(Parser *parser, int native_value);
Node parse_var_dec(Parser *parser);
Program parse(Arena *arena, Token_Arr tokens, Blocks *block_stack);
Struct get_structure(Location loc, Parser *parser, String_View name);
bool is_field(Struct *structure, String_View field);
bool is_in_function(Blocks *blocks);

#endif // FRONTEND_H
