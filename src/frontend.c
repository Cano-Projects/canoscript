#include "frontend.h"

#define TIPP_IMPLEMENTATION
#include "tipp.h"

char *token_types[TT_COUNT] = {"none", "write", "exit", "builtin", "ident", 
                               ":", "(", ")", "[", "]", "{", "}", ",", ".", 
							   "=", "==", "!=", ">=", "<=", ">", "<", "+", "-", "*", "/", "%", "&&", "||", "&",
                               "string", "char", "integer", "float", "struct", "void", "type", 
							   "if", "else", "while", "then", 
                               "return", "end", "const"};

String_View data_types[DATA_COUNT] = {
    {.data="int", .len=3},
    {.data="str", .len=3},    
    {.data="void", .len=4},        
    {.data="char", .len=4},                    
    {.data="float", .len=5},            
    {.data="double", .len=6},                		
    {.data="ptr", .len=3},                
    {.data="u8", .len=2},                		
    {.data="u16", .len=3},                	
    {.data="u32", .len=3},                
    {.data="u64", .len=3},                
};    

bool is_valid_escape(char c){
    switch(c){
        case 'n':
        case 't':
        case 'v':
        case 'b':
        case 'r':
        case 'f':
        case 'a':
        case '\\':
        case '?':
        case '\'':
        case '\"':
        case '0':
            return true;
        default:
            return false;
    }
}
	
char get_escape(char c){
    switch(c){
        case 'n':
			return '\n';
        case 't':
			return '\t';
        case 'v':
			return '\v';		
        case 'b':
			return '\b';		
        case 'r':
			return '\r';		
        case 'f':
			return '\f';				
        case 'a':
			return '\a';						
        case '\\':
			return '\\';						
        case '?':
			return '\?';								
        case '\'':
			return '\'';						
        case '\"':
			return '\"';					
        case '0':
			return '\0';							
        default:
			ASSERT(false, "unexpected escape character");
    }
}

String_View read_file_to_view(Arena *arena, char *filename) {
    FILE *file = fopen(filename, "r");
	if(file == NULL) {
		fprintf(stderr, "cannot read from file: %s\n", filename);
		exit(1);
	}
    
    fseek(file, 0, SEEK_END);
    size_t len = ftell(file);
    fseek(file, 0, SEEK_SET);    
    
    char *data = arena_alloc(arena, sizeof(char)*len);
    fread(data, sizeof(char), len, file);
    
    fclose(file);
    return (String_View){.data=data, .len=len};
}
    
bool isword(char c) {
    return isalpha(c) || isdigit(c) || c == '_';
}

bool is_valid_types(Type_Type type1, Type_Type type2) {
	if(type1 == type2 || type1 == TYPE_STR || type2 == TYPE_STR
		|| ((type1 == TYPE_INT || type1 >= TYPE_U8) && (type2 == TYPE_INT || type2 >= TYPE_U8))) {
		return true;
	}
	return false;
}
	
typedef struct {
	String_View text;
	int type;
} Keyword;

Keyword keyword_list[] = {
	{LITERAL_VIEW("write"), TT_WRITE},
	{LITERAL_VIEW("exit"), TT_EXIT},		
	{LITERAL_VIEW("if"), TT_IF},	
	{LITERAL_VIEW("else"), TT_ELSE},	
	{LITERAL_VIEW("if"), TT_IF},	
	{LITERAL_VIEW("while"), TT_WHILE},	
	{LITERAL_VIEW("then"), TT_THEN},	
	{LITERAL_VIEW("return"), TT_RET},	
	{LITERAL_VIEW("end"), TT_END},
	{LITERAL_VIEW("const"), TT_CONST},		
	{LITERAL_VIEW("struct"), TT_STRUCT},	
};
#define KEYWORDS_COUNT sizeof(keyword_list)/sizeof(*keyword_list)

Token_Type get_token_type(String_View view) {
	for(size_t i = 0; i < KEYWORDS_COUNT; i++) {
	    if(view_cmp(view, keyword_list[i].text)) {
	        return keyword_list[i].type;
		}
	}
    return TT_NONE;
}
	
Keyword builtins_list[] = {
	{LITERAL_VIEW("alloc"), BUILTIN_ALLOC},
	{LITERAL_VIEW("dealloc"), BUILTIN_DEALLOC},		
	{LITERAL_VIEW("store"), BUILTIN_STORE},	
	{LITERAL_VIEW("tovp"), BUILTIN_TOVP},		
	{LITERAL_VIEW("get"), BUILTIN_GET},	
	{LITERAL_VIEW("dll"), BUILTIN_DLL},	
	{LITERAL_VIEW("call"), BUILTIN_CALL},			
};
#define BUILTIN_COUNT sizeof(builtins_list)/sizeof(*builtins_list)

Token token_get_builtin(Token token, String_View view) {
	for(size_t i = 0; i < BUILTIN_COUNT; i++) {
	    if(view_cmp(view, builtins_list[i].text)) {
			token.type = TT_BUILTIN;
			token.value.builtin = builtins_list[i].type;
			return token;
		}
	}
    return token;
}

Token handle_data_type(Token token, String_View str) {
    for(size_t i = 0; i < DATA_COUNT; i++) {
        if(view_cmp(str, data_types[i])) {
            token.type = TT_TYPE;
            token.value.type = (Type_Type)i;
        }
    }
    return token;
}
    
bool is_operator(String_View view) {
    switch(*view.data) {
        case '+':
        case '-':
        case '*':
        case '>':
        case '<':
        case '%':
            return true;        
        case '/':
			if(view.len > 1 && view.data[1] != '/') return true;
			break;
        case '=':
            if(view.len > 1 && view.data[1] == '=') return true;        
			break;
        case '&':
            if(view.len > 1 && view.data[1] == '&') return true;        
			break;
        case '|':
            if(view.len > 1 && view.data[1] == '|') return true;        
			break;
        case '!':
            if(view.len > 1 && view.data[1] == '=') return true;        
			break;
    }
	return false;
}
    
Token create_operator_token(char *filename, size_t row, size_t col, String_View *view) {
    Token token = {0};
    token.loc = (Location){
        .filename = filename,
        .row = row,
        .col = col,
    };
    switch(*view->data) {
        case '+':
            token.type = TT_PLUS;
            break;
        case '-':
            token.type = TT_MINUS;
            break;
        case '*':
            token.type = TT_MULT;
            break;
        case '/':
            token.type = TT_DIV;
            break;
        case '%':
            token.type = TT_MOD;
            break;
        case '=':
            if(view->len > 1 && view->data[1] == '=') {
                *view = view_chop_left(*view);
                token.type = TT_DOUBLE_EQ;        
            }
            break;
        case '!':
            if(view->len > 1 && view->data[1] == '=') {
                *view = view_chop_left(*view);
                token.type = TT_NOT_EQ;        
            }
            break;
        case '>':
            if(view->len > 1 && view->data[1] == '=') {
                *view = view_chop_left(*view);
                token.type = TT_GREATER_EQ;        
            } else {
                *view = view_chop_left(*view);
                token.type = TT_GREATER;        
            }
            break;
        case '<':
            if(view->len > 1 && view->data[1] == '=') {
                *view = view_chop_left(*view);
                token.type = TT_LESS_EQ;        
            } else {
                *view = view_chop_left(*view);
                token.type = TT_LESS;
            }
            break;
        case '&':
            if(view->len > 1 && view->data[1] == '&') {
                *view = view_chop_left(*view);
                token.type = TT_AND;        
            }
            break;
        case '|':
            if(view->len > 1 && view->data[1] == '|') {
                *view = view_chop_left(*view);
                token.type = TT_OR;        
            }
            break;
        default:
            ASSERT(false, "unreachable");
    }
    return token;
}
    
void print_token_arr(Token_Arr arr) {
    for(size_t i = 0; i < arr.count; i++) {
        printf("%zu:%zu: %s, "View_Print"\n", arr.data[i].loc.row, arr.data[i].loc.col, token_types[arr.data[i].type], View_Arg(arr.data[i].value.string));
    }
}
	
Dynamic_Str generate_string(Arena *arena, String_View *view, Token token, char delim) {
    Dynamic_Str word = {0};
    *view = view_chop_left(*view);
    while(view->len > 0 && *view->data != delim) {
	    if(view->len > 1 && *view->data == '\\') {
		    *view = view_chop_left(*view);
		    if(!is_valid_escape(*view->data)) {
			    PRINT_ERROR(token.loc, "unexpected escape character: `%c`", *view->data);
		    }
			ADA_APPEND(arena, &word, get_escape(*view->data));
	    } else {
		    ADA_APPEND(arena, &word, *(*view).data);
		}
	    *view = view_chop_left(*view);
    }
	ADA_APPEND(arena, &word, '\0');		
	return word;
}

Token_Arr lex(Arena *arena, Arena *string_arena, char *entry_filename, String_View view) {
	view = prepro(entry_filename, 0);
    size_t row = 1;
    Token_Arr tokens = {0};
    const char *start = view.data;
	char *filename = entry_filename;
	while(view.len > 0) {
        Token token = {0};
        token.loc = (Location){.filename = filename, .row=row, .col=view.data-start};
        switch(*view.data) {
			case '@':
				view = view_chop_left(view);
				if(*view.data != '"') PRINT_ERROR(token.loc, "invalid preprocessor directive");
				view = view_chop_left(view);
				Dynamic_Str prepro_filename = {0};
				while(view.len > 0 && *view.data != '"') {
					ADA_APPEND(arena, &prepro_filename, *view.data);
					view = view_chop_left(view);
				}
				ADA_APPEND(arena, &prepro_filename, '\0');
				if(view.len == 0) PRINT_ERROR(token.loc, "invalid preprocessor directive");
				// consume `"` and space
				view = view_chop_left(view);
				view = view_chop_left(view);			
				Dynamic_Str line_num = {0};
				while(view.len > 0 && *view.data != '\n') {
					if(!isdigit(*view.data)) PRINT_ERROR(token.loc, "invalid preprocessor directive");
					ADA_APPEND(arena, &line_num, *view.data);
					view = view_chop_left(view);
				}
				size_t line_number = view_to_int(view_create(line_num.data, line_num.count));
				
				filename = prepro_filename.data;
				row = line_number;
				while(view.len > 0 && *view.data != '\n') view = view_chop_left(view);
				break;
            case ':':
                token.type = TT_COLON;
                ADA_APPEND(arena, &tokens, token);                                                    
                break;
            case '(':
                token.type = TT_O_PAREN;
                ADA_APPEND(arena, &tokens, token);                                                    
                break;
            case ')':
                token.type = TT_C_PAREN;
                ADA_APPEND(arena, &tokens, token);                                                    
                break;
            case '[':
                token.type = TT_O_BRACKET;
                ADA_APPEND(arena, &tokens, token);                                                    
                break;
            case ']':
                token.type = TT_C_BRACKET;
                ADA_APPEND(arena, &tokens, token);                                                    
                break;
            case '{':
                token.type = TT_O_CURLY;
                ADA_APPEND(arena, &tokens, token);                                                    
                break;
            case '}':
                token.type = TT_C_CURLY;
                ADA_APPEND(arena, &tokens, token);                                                    
                break;
            case ',':
                token.type = TT_COMMA;
                ADA_APPEND(arena, &tokens, token);                                                    
                break;
            case '.':
                token.type = TT_DOT;
                ADA_APPEND(arena, &tokens, token);                                                    
                break;
			case '"': {
				Dynamic_Str word = generate_string(string_arena, &view, token, '"');
                if(view.len == 0 && *view.data != '"') {
                    PRINT_ERROR(token.loc, "expected closing `\"`");                            
                };
                token.type = TT_STRING;
                token.value.string = view_create(word.data, word.count);
                ADA_APPEND(arena, &tokens, token);                                    
			} break;
			case '\'': {
                Dynamic_Str word = generate_string(string_arena, &view, token, '\'');
                if(word.count > 2) {
                    PRINT_ERROR(token.loc, "character cannot be made up of multiple characters");
                }
                if(view.len == 0 && *view.data != '\'') {
                    PRINT_ERROR(token.loc, "expected closing `'` quote");                            
                };
                token.type = TT_CHAR_LIT; 
                token.value.string = view_create(word.data, word.count);
                ADA_APPEND(arena, &tokens, token);                                    
			} break;
            case '\n':
                row++;
                start = view.data;
                break;
            default: {
                if(isalpha(*view.data)) {
                    Dynamic_Str word = {0};    
                    ADA_APPEND(string_arena, &word, *view.data);                    
                    view = view_chop_left(view);
                    while(view.len > 0 && isword(*view.data)) {
                        ADA_APPEND(string_arena, &word, *view.data);            
                        view = view_chop_left(view);    
                    }
                    view.data--;
                    view.len++;
                    String_View view = view_create(word.data, word.count);
                    token.type = get_token_type(view);
                    token = handle_data_type(token, view);
                    token = token_get_builtin(token, view);
                    if(token.type == TT_NONE) {
                        token.type = TT_IDENT;
                        token.value.ident = view;
                    };
                    ADA_APPEND(arena, &tokens, token);     
                } else if(isdigit(*view.data)) {
					token.type = TT_INT;
                    Dynamic_Str num = {0};
					if(*view.data == '0' && view.len > 1) {
						if(*(view.data+1) == 'x' || *(view.data+1) == 'b') {
			                view = view_chop_left(view);
							int base = *view.data == 'x' ? 16 : 2;
							view = view_chop_left(view);
			                while(view.len > 0 && isword(*view.data) && *view.data != '_') {
			                    ADA_APPEND(string_arena, &num, *view.data);            
			                    view = view_chop_left(view);    
							}
							ADA_APPEND(string_arena, &num, '\0');
							token.value.integer = strtoll(num.data, NULL, base);
		                    ADA_APPEND(arena, &tokens, token);                        
							break;
						}
					}

                    while(view.len > 0 && (isdigit(*view.data) || *view.data == '.')) {
                        if(*view.data == '.') token.type = TT_FLOAT_LIT;
                        ADA_APPEND(arena, &num, *view.data);
                        view = view_chop_left(view);
                    }
                    view.data--;
                    view.len++;
                        
                    ADA_APPEND(arena, &num, '\0');
                    if(token.type == TT_FLOAT_LIT) token.value.floating = atof(num.data);
                    else token.value.integer = atoi(num.data);
                    ADA_APPEND(arena, &tokens, token);                        
                } else if(is_operator(view)) {
                    token = create_operator_token(filename, row, view.data-start, &view);
                    ADA_APPEND(arena, &tokens, token);                                        
				} else if(*view.data == '/') {
					// We already know because of is_operator function that the next character is another forward-slash
					// So we can assume this in this block
					while(view.len > 1 && view.data[1] != '\n') {
						view = view_chop_left(view);
					}
				} else if(*view.data == '=') {
                    token.type = TT_EQ;
                    ADA_APPEND(arena, &tokens, token);                                        
                } else if(*view.data == '&') { 
	                token.type = TT_AMPERSAND;
	                ADA_APPEND(arena, &tokens, token);                                                    
				} else if(isspace(*view.data)) {

                    view = view_chop_left(view);                   
                    continue;
                } else {
					if(*view.data == '\0') {
						fprintf(stderr, "%s:%zu:%zu NOTE: Ignoring null-byte\n", token.loc.filename, token.loc.row, token.loc.col);
						view = view_chop_left(view);
						continue;
					}
                    PRINT_ERROR(token.loc, "unexpected token `%c`", *view.data);
                }
            }
        }
        view = view_chop_left(view);               
    }
    return tokens;
}
    
Token token_consume(Token_Arr *tokens) {
    ASSERT(tokens->count != 0, "out of tokens");
    tokens->count--;
    return *tokens->data++;
}

Token token_peek(Token_Arr *tokens, size_t peek_by) {
    if(tokens->count <= peek_by) {
        return (Token){0};
    }
    return tokens->data[peek_by];
}

Token expect_token(Token_Arr *tokens, Token_Type type) {
    Token token = token_consume(tokens);
    if(token.type != type) {
        PRINT_ERROR(token.loc, "expected type: `%s`, but found type `%s`", 
                    token_types[type], token_types[token.type]);
    };
	return token;
}

Node *create_node(Arena *arena, Node_Type type) {
    Node *node = arena_alloc(arena, sizeof(Node));
    node->type = type;
    return node;
}
    
Precedence op_get_prec(Token_Type type) {
    switch(type) {
        case TT_PLUS:
        case TT_MINUS:
        case TT_DOUBLE_EQ:
        case TT_NOT_EQ:
        case TT_GREATER_EQ:
        case TT_LESS_EQ:
        case TT_GREATER:
        case TT_LESS:
            return PREC_1;
        case TT_MULT:
        case TT_DIV:
        case TT_MOD:
		case TT_AND:
		case TT_OR:
            return PREC_2;
            break;
        default:
            return PREC_0;
    }
}

Operator create_operator(Token_Type type) {
    Operator op = {0};
    op.precedence = op_get_prec(type);
    switch(type) {
        case TT_PLUS:
            op.type = OP_PLUS;
            break;        
        case TT_MINUS:
            op.type = OP_MINUS;
            break;        
        case TT_MULT:
            op.type = OP_MULT;
            break;        
        case TT_DIV:
            op.type = OP_DIV;
            break;
        case TT_MOD:
            op.type = OP_MOD;
            break;
        case TT_DOUBLE_EQ:
            op.type = OP_EQ;
            break;
        case TT_NOT_EQ:
            op.type = OP_NOT_EQ;
            break;
        case TT_GREATER_EQ:
            op.type = OP_GREATER_EQ;
            break;
        case TT_LESS_EQ:
            op.type = OP_LESS_EQ;
            break;
        case TT_GREATER:
            op.type = OP_GREATER;
            break;
        case TT_LESS:
            op.type = OP_LESS;
            break;
        case TT_AND:
            op.type = OP_AND;
            break;
        case TT_OR:
            op.type = OP_OR;
            break;
        default:
            return (Operator){0};
    }
    return op;
}

bool token_is_op(Token token) {
    switch(token.type) {
        case TT_PLUS:
        case TT_MINUS:
        case TT_MULT:
        case TT_DIV:
		case TT_AND:
		case TT_OR:
            return true;    
        default:
            return false;
    }
}

void print_expr(Expr *expr) {
    if(expr->type == EXPR_INT) {
        printf("int: %d\n", expr->value.integer);
    } else {
        print_expr(expr->value.bin.lhs);
        print_expr(expr->value.bin.rhs);        
    }
}

Expr *parse_primary(Parser *parser) {
	Arena *arena = parser->arena;
	Token_Arr *tokens = parser->tokens;
	Token token = token_consume(tokens);
	Expr *expr = malloc(sizeof(Expr));
	memset(expr, 0, sizeof(Expr));
    switch(token.type) {
        case TT_INT:
            *expr = (Expr){
                .type = EXPR_INT,
				.data_type = TYPE_INT,
                .value.integer = token.value.integer,
				.loc = token.loc,
            };
            break;
        case TT_FLOAT_LIT:
            *expr = (Expr){
                .type = EXPR_FLOAT,
				.data_type = TYPE_FLOAT,
                .value.floating = token.value.floating,
				.loc = token.loc,
            };
            break;
        case TT_STRING:
            *expr = (Expr){
                .type = EXPR_STR,
				.data_type = TYPE_CHAR,
                .value.string = token.value.string,
				.loc = token.loc,
            };
            break;
        case TT_CHAR_LIT:
            *expr = (Expr){
                .type = EXPR_CHAR,
				.data_type = TYPE_CHAR,
                .value.string = token.value.string,
				.loc = token.loc,
            };
            break;
        default:
            return NULL;
    }
	expr->loc = token.loc;
    return expr;
}

    
Expr *parse_expr_1(Parser *parser, Expr *lhs, Precedence min_precedence) {
	Arena *arena = parser->arena;
	Token_Arr *tokens = parser->tokens;
    Token lookahead = token_peek(tokens, 0);
    // make sure it's an operator
    while(op_get_prec(lookahead.type) >= min_precedence) {
        Operator op = create_operator(lookahead.type);    
        if(tokens->count > 0) {
            token_consume(tokens);
            Expr *rhs = parse_primary(parser);
			if(!is_valid_types(lhs->data_type, rhs->data_type)) {
				PRINT_ERROR(rhs->loc, "expression with types of both %s and %s", 
									  data_types[lhs->data_type].data, data_types[rhs->data_type].data);
			}
            lookahead = token_peek(tokens, 0);
            while(op_get_prec(lookahead.type) > op.precedence) {
                rhs = parse_expr_1(parser, rhs, op.precedence+1);
                lookahead = token_peek(tokens, 0);
            }
            // allocate new lhs node to ensure old lhs does not point
            // at itself, getting stuck in a loop
            Expr *new_lhs = arena_alloc(arena, sizeof(Expr));
            *new_lhs = (Expr) {
                .type = EXPR_BIN,
				.data_type = lhs->data_type,
                .value.bin.lhs = lhs,
                .value.bin.rhs = rhs,
                .value.bin.op = op,
				.loc = lhs->loc,
            };
            lhs = new_lhs;
        }
    }
    return lhs;
}
			
Expr *parse_expr(Parser *parser) {
    return parse_expr_1(parser, parse_primary(parser), 1);
}

Type_Type get_type(Token token) {
    if(token.type != TT_TYPE) {
        fprintf(stderr, "Error: not a valid type\n");
        exit(1);
    }
    return token.value.type; 
}

Arg_List parse_arg_list(Parser *parser) {
    Arg_List args = {0};
    bool running = true;
    while(running) {
        DA_APPEND(&args, parse_expr(parser));
        Token token = token_consume(parser->tokens);
        if(token.type == TT_C_PAREN)
            running = false;
        else if(token.type != TT_COMMA) {
            fprintf(stderr, "Error: expected comma\n");
            exit(1);
        }
    }
    return args;
}

Statement *parse_var_dec(Parser *parser, Token identifier) {
    Statement *stmt = arena_alloc(parser->arena, sizeof(Statement));
    stmt->type = STMT_VAR_DEC;
    Var var = {.name = identifier.value.ident};
    var.type = get_type(token_consume(parser->tokens));
    stmt->val.var = var;
    expect_token(parser->tokens, TT_EQ);
    stmt->expr = parse_expr(parser);
    if(!stmt->expr) {
        fprintf(stderr, "Error: invalid expression\n");
        exit(1);
    }
    return stmt;
}

Statement *parse_reassign(Parser *parser, Token identifier) {
    Statement *stmt = arena_alloc(parser->arena, sizeof(Statement));
    stmt->type = STMT_REASSIGN;
    stmt->val.var = (Var){.name = identifier.value.ident};
    stmt->expr = parse_expr(parser);
    if(!stmt->expr) {
        fprintf(stderr, "Error: invalid expression\n");
        exit(1);
    }
    return stmt;
}

Statement *parse_func_call(Parser *parser, Token identifier) {
    Statement *stmt = arena_alloc(parser->arena, sizeof(Statement));
    stmt->type = STMT_FUNC_CALL; 
    stmt->val.call.name = identifier.value.ident;
    stmt->val.call.args = parse_arg_list(parser);
    return stmt;
}

Statement *parse_stmt(Parser *parser) {
    Statement *stmt = NULL;
    Token start_token = token_consume(parser->tokens); 
    switch(token_consume(parser->tokens).type) {
        case TT_COLON:
            stmt = parse_var_dec(parser, start_token);
            break;
        case TT_EQ:
            stmt = parse_reassign(parser, start_token);
            break;
        case TT_O_PAREN:
            stmt = parse_func_call(parser, start_token);
            break;
        default:
            ASSERT(false, "Unreachable");
    }
    printf("Type is: %d\n", stmt->type);
    return stmt;
}
    
Program parse(Arena *arena, Token_Arr tokens) {
    Parser parser = {0};
    parser.tokens = &tokens;
    parser.arena = arena;
	while(tokens.count > 0) {
		switch(parser.tokens->data[0].type) {
            case TT_IDENT:
                switch(token_peek(parser.tokens, 1).type) {
                    case TT_O_PAREN:
                        parse_stmt(&parser);
                        // TODO: detect function declaration and call appropriately
                        break;
                    case TT_COLON:
                    case TT_EQ: {
                        Statement *stmt = parse_stmt(&parser);
                        if(!stmt) {
                            fprintf(stderr, "Error: Statement was invalid\n");
                            exit(1);
                        }
                    } break;
                    default:
                        fprintf(stderr, "Unexpected token: %s\n", token_types[parser.tokens->data[0].type]);
                        exit(1);
                }
                break;
            case TT_IF:
                break;
            case TT_WHILE:
                break;
            default:
                fprintf(stderr, "Invalid identifier: %s\n", token_types[parser.tokens->data[0].type]);
                exit(1);
        }		
	}			
    return (Program){0};
}
