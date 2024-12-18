#pragma once
#ifndef DEFS_H
#define DEFS_H

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

#include "view.h"
#include "arena.h"

#define DATA_START_CAPACITY 16

#define NATIVE_OPEN 0
#define NATIVE_WRITE 0
#define NATIVE_EXIT 1

#define STDOUT 1

#define ASSERT(cond, ...) \
    do { \
        if (!(cond)) { \
            fprintf(stderr, "%s:%d: ASSERTION FAILED: ", __FILE__, __LINE__); \
            fprintf(stderr, __VA_ARGS__); \
            fprintf(stderr, "\n"); \
            exit(1); \
        } \
    } while (0)

#define ADA_APPEND(arena, da, item) do {                                                       \
    if ((da)->count >= (da)->capacity) {                                               \
        (da)->capacity = (da)->capacity == 0 ? DATA_START_CAPACITY : (da)->capacity*2; \
        (da)->data = arena_realloc((arena), (da)->data, (da)->count*sizeof(*(da)->data), (da)->capacity*sizeof(*(da)->data));       \
        ASSERT((da)->data != NULL, "outta ram");                               \
    }                                                                                  \
    (da)->data[(da)->count++] = (item);                                               \
} while (0)
	
#define DA_APPEND(da, item) do {                                                       \
    if ((da)->count >= (da)->capacity) {                                               \
        (da)->capacity = (da)->capacity == 0 ? DATA_START_CAPACITY : (da)->capacity*2; \
        (da)->data = custom_realloc((da)->data, (da)->capacity*sizeof(*(da)->data));       \
        ASSERT((da)->data != NULL, "outta ram");                               \
    }                                                                                  \
    (da)->data[(da)->count++] = (item);                                               \
} while (0)
    
#define PRINT_ERROR(loc, ...)                                                 \
    do {                                                                           \
		ASSERT((loc).filename != NULL, "Filename cannot be NULL");			\
        fprintf(stderr, "%s:%zu:%zu: error: ", loc.filename, loc.row, loc.col);  \
        fprintf(stderr, __VA_ARGS__);    \
        fprintf(stderr, "\n"); \
        exit(1);                                                                   \
    } while(0)
    
typedef enum {
    BUILTIN_ALLOC = 0,
    BUILTIN_DEALLOC,
    BUILTIN_STORE,
    BUILTIN_TOVP,
    BUILTIN_GET,        
	BUILTIN_DLL,
	BUILTIN_CALL,	
} Builtin_Type;
    
typedef struct {
    char *data;
    size_t count;
    size_t capacity;
} Dynamic_Str;
    
typedef enum {
    TYPE_INT,
    TYPE_STR,
    TYPE_VOID,
    TYPE_CHAR,
    TYPE_FLOAT,
	TYPE_DOUBLE,
    TYPE_PTR,
	TYPE_U8,
	TYPE_U16,
	TYPE_U32,
	TYPE_U64,
    DATA_COUNT,
} Type_Type;

typedef struct {
    size_t row;
    size_t col;
    char *filename;
} Location;

typedef enum {
    OP_PLUS,
    OP_MINUS,
    OP_MULT,
    OP_DIV,
    OP_MOD,
    OP_EQ,
    OP_NOT_EQ,
    OP_GREATER_EQ,
    OP_LESS_EQ,
    OP_GREATER,
    OP_LESS,
	OP_AND,
	OP_OR,
} Operator_Type;
    
typedef enum {
    PREC_0 = 0,
    PREC_1,
    PREC_2,
    PREC_COUNT,
} Precedence;
   
typedef struct {
    Operator_Type type;
    Precedence precedence;
} Operator;

struct Expr;    
struct Node;

typedef struct {
    struct Expr *lhs;
    struct Expr *rhs;
    Operator op;
} Bin_Expr;
    
typedef struct {
    struct Expr **data;
    size_t count;
    size_t capacity;
} Exprs;
	
typedef struct {
	Type_Type *data;
	size_t count;
	size_t capacity;
} Data_Types;
	
typedef struct {
	Type_Type type;
	bool is_struct;
	bool is_ptr;
	String_View struct_name;
} Ext_Arg;
	
typedef struct {
	Ext_Arg *data;
	size_t count;
	size_t capacity;
} Ext_Args;

typedef struct {
	String_View file_name;
	String_View name;
	Ext_Args args;
	Type_Type return_type;
} Ext_Func;
	
typedef struct {
	String_View name;
	Exprs args;
	Type_Type return_type;
} Ext_Func_Call;

typedef struct {
	Ext_Func *data;
	size_t count;
	size_t capacity;
	String_View file_name;
} Ext_Funcs;
    
typedef struct {
    Builtin_Type type;
    Exprs value;
    Type_Type return_type;
	// optional
	Ext_Funcs ext_funcs;	
} Builtin;
    
typedef struct {
    String_View name;
    struct Expr *index;
    String_View var_name;	
} Array;
    
typedef struct {
    String_View structure;
    String_View var_name;
    Exprs value;
} Field;

typedef enum {
    EXPR_BIN,    
    EXPR_INT,
    EXPR_FLOAT,
    EXPR_STR,
    EXPR_CHAR,
    EXPR_VAR,
    EXPR_FUNCALL,
    EXPR_ARR,
	EXPR_STRUCT,
    EXPR_FIELD,
	EXPR_FIELD_ARR,
    EXPR_BUILTIN,
	EXPR_EXT,
    EXPR_COUNT,
} Expr_Type;
	
typedef struct {
    struct Node *data;
    size_t count;
    size_t capacity;
} Nodes;
	
typedef struct {
    String_View name;
    Nodes values;
} Struct;
	
typedef struct {
	String_View name;
	Exprs values;
} Structure;

typedef union {
    Bin_Expr bin;
    int integer;
    double floating;
    Field field;
	Structure structure;
    Array array;
    String_View variable;
    String_View string;
	Ext_Func_Call ext;
    Builtin builtin;
} Expr_Value;

typedef struct Expr {
    Expr_Value value;
    Expr_Type type;   
    Type_Type return_type;
	Type_Type data_type;
    Location loc;
} Expr;

typedef enum {
    VAR_STRING,
    VAR_INT,
} Var_Type;

typedef struct {
    String_View name;
    Nodes args;
    Type_Type type;
    Nodes body;
    size_t label;
} Func_Dec;
    
// TODO: rename TYPE_* to NODE_*
typedef enum {
    TYPE_ROOT = 0,
    TYPE_NATIVE,
    TYPE_EXPR,
    TYPE_EXPR_STMT,
    TYPE_VAR_DEC,
    TYPE_VAR_REASSIGN,
    TYPE_FIELD_REASSIGN,
    TYPE_IF,
    TYPE_ELSE,
    TYPE_WHILE,
    TYPE_THEN,
    TYPE_FUNC_DEC,
    TYPE_FUNC_CALL,
	TYPE_EXT_FUNC,
    TYPE_STRUCT,
    TYPE_ARR_INDEX,
    TYPE_RET,
    TYPE_END,
    TYPE_COUNT,
} Node_Type;
    
typedef struct {
    String_View name;
    String_View struct_name;
	String_View function;	
    Type_Type type;
    Exprs value;
    size_t stack_pos;
    bool is_array;
    bool is_struct;
	bool global;	
	bool is_const;
    Expr *array_s;
} Variable;
    
typedef struct {
    String_View name;
    Nodes args;
    Type_Type type;
	size_t label;	
} Function;
	
typedef struct {
	Function *data;
	size_t count;
	size_t capacity;
} Functions;

typedef struct {
    Variable *data;
    size_t count;
    size_t capacity;
} Variables;
    
typedef struct {
    String_View name;
    Expr *index;
    Exprs value;
} Array_Index;
    
typedef struct {
    size_t label1;
    size_t label2;
} Else_Label;
    
typedef struct {
    size_t num;
    String_View function;
} Label;

typedef union {
    Expr *expr;
    Expr *conditional;
    Expr *expr_stmt;
    Variable var;
    Array_Index array;
    Label label;
    Else_Label el;
    Func_Dec func_dec;
    Struct structs;
    Field field;
} Node_Value;

typedef struct Node {
    Node_Type type;
    Node_Value value;
    Location loc;
} Node;

typedef enum {
    BLOCK_IF,
    BLOCK_ELSE,
    BLOCK_WHILE,
    BLOCK_FUNC,
} Block_Type;
    
typedef struct {
    Block_Type *data;
    size_t count;
    size_t capacity;    
} Block_Stack;

typedef struct {
    size_t *data;
    size_t count;
    size_t capacity;
} Size_Stack;

typedef union {
	Function function;
	Variable var;
	Struct structure;
	Ext_Func ext;
} Symbol_Value;
	
typedef enum {
	SYMBOL_FUNC,
	SYMBOL_VAR,
	SYMBOL_STRUCT,
	SYMBOL_EXT,
} Symbol_Type;
	
typedef struct {
	Symbol_Value val;
	Symbol_Type type;
} Symbol;

typedef struct {
	Symbol *data;
	size_t count;
	size_t capacity;
} Symbols;

typedef struct {
    Nodes nodes;  
    Functions functions;
    Nodes structs;
	Nodes vars;
	Nodes ext_nodes;
	Symbols symbols;
} Program;

typedef enum {
    TT_NONE = 0,
    TT_WRITE,
    TT_EXIT,
    TT_BUILTIN,
    TT_IDENT,
    TT_COLON,
    TT_O_PAREN,
    TT_C_PAREN,
    TT_O_BRACKET,
    TT_C_BRACKET,
    TT_O_CURLY,
    TT_C_CURLY,
    TT_COMMA,
    TT_DOT,
    TT_EQ,
    TT_DOUBLE_EQ,
    TT_NOT_EQ,
    TT_GREATER_EQ,
    TT_LESS_EQ,
    TT_GREATER,
    TT_LESS,
    TT_PLUS,
    TT_MINUS,
    TT_MULT,
    TT_DIV,
    TT_MOD,
	TT_AND,
	TT_OR,
	TT_AMPERSAND,
    TT_STRING,
    TT_CHAR_LIT,
    // TODO TT_INT_LIT
    TT_INT,
    TT_FLOAT_LIT,
    TT_STRUCT,
    TT_VOID,
    TT_TYPE,
    TT_IF,
    TT_ELSE,
    TT_WHILE,
    TT_THEN,
    TT_RET,
    TT_END,
	TT_CONST,
    TT_COUNT,
} Token_Type;

typedef union {
    String_View string;
    String_View ident;
    int integer;
    double floating;
    Type_Type type;
    Builtin_Type builtin;
} Token_Value;

typedef struct {
    Token_Value value; 
    Token_Type type;
    Location loc;
} Token;
    
typedef struct {
    Token *data;
    size_t count;
    size_t capacity;
} Token_Arr;

typedef struct {
	Arena *arena;
	Token_Arr *tokens;
	Nodes *variables;
	Nodes ext_nodes;
	Functions *functions;
	Nodes *structs;
	Symbols symbols;
} Parser;

typedef enum {
    STMT_VAR_DEC,
    STMT_REASSIGN,
    STMT_IF,
    STMT_WHILE,
    STMT_FUNC_CALL,
    STMT_BUILTIN,
} Statement_Type;

struct Statement;

typedef struct {
    struct Statement *data;
    size_t count;
    size_t capacity;
} Block;

typedef struct {
    String_View name;
    Type_Type type;
} Var;

typedef struct {
    Expr **data;
    size_t count;
    size_t capacity;
} Arg_List;

typedef struct {
    Arg_List args;  
    String_View name;
} Func_Call;

typedef union {
    Var var;
    Block *block;
    Func_Call call;
} Statement_Val;

typedef struct {
    Statement_Type type; 
    Statement_Val val; 
    Expr *expr;
} Statement;

void *custom_realloc(void *ptr, size_t size);
    
#endif // DEFS_H
