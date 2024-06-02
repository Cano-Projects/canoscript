#include "backend.h"
#include <errno.h>

char *node_types[TYPE_COUNT] = {"root", "native", "expr", "var_dec", "var_reassign",
                                "if", "else", "while", "then", "func_dec", "func_call", "return", "end"};
	
char *data_typess[DATA_COUNT] = {
    "int",
    "pointer", 
    "void",
    "char",                    
    "float",            
    "double",            
    "pointer",
    "u8",
	"u16",                	
    "u32",                	
    "u64",                		
};    
	
char *data_typesss[DATA_COUNT] = {
    "INT",
    "PTR", 
    "THIS_IS_WRONG",
    "CHAR",                    
    "FLOAT",            
    "DOUBLE",            
    "PTR",
    "U8",
    "U16",
	"U32",
    "U64",                	
};    
	
DataType type_to_data[DATA_COUNT] = {
    INT_TYPE,
    PTR_TYPE, 
	0,
    CHAR_TYPE,                    
    FLOAT_TYPE,            
    DOUBLE_TYPE,            
    PTR_TYPE,
    U8_TYPE,
    U16_TYPE,
	U32_TYPE,
	U64_TYPE,                	
};    

size_t data_type_s[DATA_COUNT] = {8, 1, 1, 1, 4, 8, 8, 1, 2, 4, 8};

Node get_struct(Nodes structs, String_View name) {
    for(size_t i = 0; i < structs.count; i++) {
        if(view_cmp(name, structs.data[i].value.structs.name)) {
            return structs.data[i];
        }
    }
    ASSERT(false, "unknown struct: "View_Print, View_Arg(name));
    PRINT_ERROR((Location){0}, "unknown struct\n");
}

Inst create_inst(Inst_Set type, Word value, DataType d_type) {
	return (Inst) {
		.type = type,
		.value = value,
		.data_type = d_type,
	};
}
    
// TODO: add ASSERTs to all "popping" functions
void gen_push(Program_State *state, int value) {
	Inst inst = create_inst(INST_PUSH, (Word){.as_int=value}, INT_TYPE);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s++;   
}
	
void gen_push_u(Program_State *state, uint64_t value, DataType type) {
	Inst inst = create_inst(INST_PUSH, (Word){.as_u64=value}, type);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s++;   
}
	
void gen_native(Program_State *state, int value) {
	Inst inst = create_inst(INST_NATIVE, (Word){.as_int=value}, INT_TYPE);
	DA_APPEND(&state->machine.instructions, inst);
}
    
void gen_push_float(Program_State *state, double value) {
	Inst inst = create_inst(INST_PUSH, (Word){.as_float=value}, FLOAT_TYPE);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s++;   
}
    
void gen_push_char(Program_State *state, String_View value) {
	Inst inst = create_inst(INST_PUSH, (Word){.as_char=value.data[0]}, CHAR_TYPE);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s++;   
}
    
void gen_pop(Program_State *state) {
	Inst inst = create_inst(INST_POP, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s--;   
}
    
void gen_add(Program_State *state) {
	Inst inst = create_inst(INST_ADD, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s--;   
}
    
void gen_sub(Program_State *state) {
	Inst inst = create_inst(INST_SUB, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s--;   
}

void gen_mul(Program_State *state) {
	Inst inst = create_inst(INST_MUL, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s--;   
}

void gen_div(Program_State *state) {
	Inst inst = create_inst(INST_DIV, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s--;   
}

void gen_push_str(Program_State *state, String_View value) {
	Inst inst = create_inst(INST_PUSH_STR, (Word){.as_int=state->machine.str_stack.count}, INT_TYPE);
	DA_APPEND(&state->machine.instructions, inst);
	DA_APPEND(&state->machine.str_stack, value);
    state->stack_s += 1;
}

void gen_ss(Program_State *state) {
	Inst inst = create_inst(INST_SS, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
	state->stack_s++;
}
    
void gen_indup(Program_State *state, size_t value) {
	gen_push(state, value);
	Inst inst = create_inst(INST_INDUP, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
}
	
void gen_global_indup(Program_State *state, size_t value) {
	gen_ss(state);
	gen_push(state, value);
	gen_sub(state);
	Inst inst = create_inst(INST_INDUP, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
}
	
void gen_inswap(Program_State *state, size_t value) {
	gen_push(state, value);
	Inst inst = create_inst(INST_INSWAP, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
	state->stack_s--;
}
	
void gen_global_inswap(Program_State *state, size_t value) {
	gen_ss(state);
	gen_push(state, value);
	gen_sub(state);
	Inst inst = create_inst(INST_INSWAP, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
	state->stack_s--;
}
    
void gen_zjmp(Program_State *state, size_t label) {
	Inst inst = create_inst(INST_ZJMP, (Word){.as_int=label}, INT_TYPE);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s--;
}
    
void gen_jmp(Program_State *state, size_t label) {
	Inst inst = create_inst(INST_JMP, (Word){.as_int=label}, INT_TYPE);
	DA_APPEND(&state->machine.instructions, inst);
}
    
void gen_while_jmp(Program_State *state, size_t label) {
	Inst inst = create_inst(INST_JMP, (Word){.as_int=label}, PTR_TYPE);
	DA_APPEND(&state->machine.instructions, inst);
}

void gen_label(Program_State *state, size_t label) {
	Inst inst = create_inst(INST_NOP, (Word){.as_int=0}, 0);
	while(state->labels.count <= label) DA_APPEND(&state->labels, 0);
	state->labels.data[label] = state->machine.instructions.count;	
	DA_APPEND(&state->machine.instructions, inst);	
}
    
void gen_func_label(Program_State *state, String_View label) {
	Inst inst = create_inst(INST_NOP, (Word){.as_int=0}, 0);
	Function *function = get_func(state->functions, label);
	function->label = state->machine.instructions.count;
	DA_APPEND(&state->machine.instructions, inst);	
}
    
void gen_func_call(Program_State *state, String_View label) {
	size_t loc = get_func_loc(state->program.functions, label);
	Inst inst = create_inst(INST_CALL, (Word){.as_int=loc}, INT_TYPE);
	DA_APPEND(&state->machine.instructions, inst);
}
    
void gen_while_label(Program_State *state, size_t label) {
	Inst inst = create_inst(INST_NOP, (Word){.as_int=0}, 0);
	while(state->while_labels.count <= label) DA_APPEND(&state->while_labels, 0);
	state->while_labels.data[label] = state->machine.instructions.count;	
	DA_APPEND(&state->machine.instructions, inst);	
}
    
void gen_alloc(Program_State *state, Expr *s, size_t type_s) {
    gen_push(state, type_s);
    gen_expr(state, s);
    gen_mul(state);
	Inst inst = create_inst(INST_ALLOC, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
}
    
void gen_struct_alloc(Program_State *state, size_t total_s) {
    gen_push(state, total_s);
	Inst inst = create_inst(INST_ALLOC, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
}

void gen_dup(Program_State *state) {
	Inst inst = create_inst(INST_DUP, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s++;
}

void gen_offset(Program_State *state, size_t offset) {
    gen_push(state, offset);
    gen_add(state);
	Inst inst = create_inst(INST_TOVP, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
}

void gen_write(Program_State *state) {
	Inst inst = create_inst(INST_WRITE, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s -= 3;
}
    
void gen_read(Program_State *state) {
	Inst inst = create_inst(INST_READ, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
    state->stack_s -= 1;
}
    
void gen_arr_offset(Program_State *state, size_t var_index, Expr *arr_index, Type_Type type) {
    gen_indup(state, state->stack_s-var_index);    
    gen_expr(state, arr_index);
    gen_push(state, data_type_s[type]);            
    gen_mul(state);
    gen_add(state);
	Inst inst = create_inst(INST_TOVP, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
}

void gen_struct_offset(Program_State *state, Type_Type type, size_t offset) {
    gen_dup(state);
    gen_push(state, offset);
    gen_push(state, data_type_s[type]);            
    gen_mul(state);
    gen_add(state);
	Inst inst = create_inst(INST_TOVP, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
}
    
void gen_struct_value(Program_State *state, size_t field_pos, Node *field, Node *value) {
    for(size_t i = 0; i < value->value.var.struct_value.count; i++) {
        Arg var = value->value.var.struct_value.data[i];
        if(view_cmp(field->value.var.name, var.name)) {
            gen_struct_offset(state, field->value.var.type, field_pos);
            gen_expr(state, var.value.expr);
            gen_push(state, data_type_s[field->value.var.type]);
            gen_write(state);
        }
    }
}

void gen_structure_field(Program_State *state, size_t offset, Expr *expr) {
	gen_dup(state);
	gen_push(state, offset);
	gen_add(state);
	Inst inst = create_inst(INST_TOVP, (Word){.as_int=0}, 0);
	DA_APPEND(&state->machine.instructions, inst);
	gen_expr(state, expr);
	gen_push(state, data_type_s[expr->data_type]);
	gen_write(state);
}
    
void strip_off_dot(char *str) {
    while(*str != '\0' && *str != '.') {
        str++;
    }
    *str = '\0';
}

char *append_ext(char *filename, char *ext) {
    size_t filename_s = strlen(filename);
    char *output = custom_realloc(NULL, sizeof(char)*filename_s);
    memcpy(output, filename, filename_s);
    strip_off_dot(output);
    char *output_filename = custom_realloc(NULL, sizeof(char)*strlen(output)+strlen(ext)+1);
    sprintf(output_filename, "%s.%s", output, ext);
	free(output);
    return output_filename;
}
    
char *op_types[] = {"add", "sub", "mul", "div", "mod", "cmpe", "cmpne", "cmpge", "cmple", "cmpg", "cmpl", "and", "or"};
Inst_Set op_types_inst[] = {INST_ADD, INST_SUB, INST_MUL, INST_DIV, INST_MOD, INST_CMPE, 
						 INST_CMPNE, INST_CMPGE, INST_CMPLE, INST_CMPG, INST_CMPL, INST_AND, INST_OR};

Function *get_func(Functions functions, String_View name) {
    for(size_t i = 0; i < functions.count; i++) {
        if(view_cmp(functions.data[i].name, name)) {
            return &functions.data[i];
        }
    }
    return false;
}
	
size_t get_func_loc(Functions functions, String_View name) {
    for(size_t i = 0; i < functions.count; i++) {
        if(view_cmp(functions.data[i].name, name)) {
            return i;
        }
    }
	ASSERT(false, "unexpected function name: "View_Print"\n", View_Arg(name));
}
	
bool is_func_arg(Program_State *state, String_View name) {
	if(state->functions.count == 0) return false;
	size_t index = state->functions.count-1;
	for(size_t i = 0; i < state->functions.data[index].args.count; i++) {
		if(view_cmp(name, state->functions.data[index].args.data[i].value.var.name)) return true;
	}
	return false;
}
	

bool is_current_func(Program_State *state, String_View func) {
	if(state->functions.count == 0) return false;
	if(view_cmp(func, state->functions.data[state->functions.count-1].name)) return true;
	return false;
	
}

int get_variable_location(Program_State *state, String_View name) {
    for(size_t i = 0; i < state->vars.count; i++) {
        if(view_cmp(state->vars.data[i].name, name) && (state->vars.data[i].global || 
			is_func_arg(state, name) || true ||
			is_current_func(state, state->vars.data[i].function)
			)) {
            return state->vars.data[i].stack_pos;
        }
    }
    return -1;
}
    
Variable get_variable(Program_State *state, String_View name) {
    for(size_t i = 0; i < state->vars.count; i++) {
        if(view_cmp(state->vars.data[i].name, name)) {
            return state->vars.data[i];
        }
    }
    ASSERT(false, "unknown variable: "View_Print, View_Arg(name));
}
    
Type_Type get_variable_type(Program_State *state, String_View name) {
	return get_variable(state, name).type;
}
	
Ext_Funcs gen_ext_func_wrapper(Ext_Funcs funcs, Location loc) {
	char *output_name = malloc(sizeof(char)*256);
	sprintf(output_name, View_Print".c", View_Arg(funcs.file_name));
	ASSERT(access(output_name, F_OK) == 0, "file does not exist");		
	FILE *file = fopen(output_name, "a");		
	if(file == NULL) PRINT_ERROR(loc, "Could not open file "View_Print"\n", View_Arg(funcs.data[0].file_name));
	Ext_Funcs new_funcs = {0};
	for(size_t i = 0; i < funcs.count; i++) {
		Ext_Func func = funcs.data[i];
		fprintf(file, "void native_"View_Print"(Machine *machine) {\n", View_Arg(func.name));
		if(func.args.count == 0 && func.return_type == TYPE_VOID) {
			fprintf(file, "(void)machine;");
		}
		for(int i = func.args.count-1; i >= 0; i--) {
			fprintf(file, "\tData %c = pop(machine);\n", (char)i+'a');
		}
		if(func.return_type == TYPE_VOID) fprintf(file, View_Print"(", View_Arg(func.name));
		else fprintf(file, "\t%s result = "View_Print"(", data_typess[func.return_type], View_Arg(func.name));
		for(size_t i = 0; i < func.args.count; i++) {
			if(func.args.data[i].is_struct && !func.args.data[i].is_ptr) {
				fprintf(file, "*(native_"View_Print"*)", View_Arg(func.args.data[i].struct_name));
			}
			fprintf(file, "%c.word.as_%s", (char)i+'a', data_typess[func.args.data[i].type]);
			if(i != func.args.count-1) fprintf(file, ", ");		
		}
		fprintf(file, ");\n");	
		if(func.return_type != TYPE_VOID) {
			fprintf(file, "\tpush(machine, (Word){.as_%s=result}, %s_TYPE);\n", 
							data_typess[func.return_type], data_typesss[func.return_type]);
		}
		fprintf(file, "}\n");
		char *func_name = malloc(sizeof(char)*128);
		sprintf(func_name, "native_"View_Print, View_Arg(func.name));
		Ext_Func new_func = {
			.file_name = {output_name, funcs.file_name.len+sizeof(".c")-1},
			.name = {func_name, func.name.len+sizeof("native_")-1},
		};
		DA_APPEND(&new_funcs, new_func);
	}
	fclose(file);		
	return new_funcs;
}

void gen_builtin(Program_State *state, Expr *expr) {
    ASSERT(expr->type == EXPR_BUILTIN, "type is incorrect");
    for(size_t i = 0; i < expr->value.builtin.value.count; i++) {
         gen_expr(state, expr->value.builtin.value.data[i]);
    }
    switch(expr->value.builtin.type) {
        case BUILTIN_ALLOC: {
			Inst inst = create_inst(INST_ALLOC, (Word){.as_int=0}, 0);
			DA_APPEND(&state->machine.instructions, inst);
        } break;
        case BUILTIN_DEALLOC: {
			Inst inst = create_inst(INST_DEALLOC, (Word){.as_int=0}, 0);
			DA_APPEND(&state->machine.instructions, inst);
            state->stack_s--;
        } break;
        case BUILTIN_TOVP: {
			Inst inst = create_inst(INST_TOVP, (Word){.as_int=0}, 0);
			DA_APPEND(&state->machine.instructions, inst);
        } break;
        case BUILTIN_STORE: {
            if(expr->value.builtin.value.count != 3) {
                PRINT_ERROR(expr->loc, "incorrect arg amounts for store");
            }
			Inst inst = create_inst(INST_WRITE, (Word){.as_int=0}, 0);
			DA_APPEND(&state->machine.instructions, inst);
            state->stack_s -= 3;
        } break;
        case BUILTIN_GET: {
            if(expr->value.builtin.value.count != 2) {
                PRINT_ERROR(expr->loc, "incorrect arg amounts for get");
            }
			Inst inst = create_inst(INST_READ, (Word){.as_int=0}, 0);
			DA_APPEND(&state->machine.instructions, inst);
            state->stack_s -= 1;
        } break;
        case BUILTIN_DLL: {
			Ext_Funcs new_funcs = gen_ext_func_wrapper(expr->value.builtin.ext_funcs, expr->loc);
			char *output = malloc(sizeof(char)*128);
			sprintf(output, View_Print".so", View_Arg(new_funcs.data[0].file_name));
			
			char command[1024] = {0};
			sprintf(command, "gcc -L. -Wall -Wextra -Wno-implicit-function-declaration -fPIC -shared -o %s "View_Print" "View_Print, 
				output,
				View_Arg(new_funcs.data[0].file_name),
				View_Arg(expr->value.builtin.ext_funcs.file_name));
				
			if(system(command) != 0) {
				printf("Command failed!\n");
				exit(1);
			}
			//gen_push_str(state, (String_View){"\0", 1});											
			for(size_t i = 0; i < new_funcs.count; i++) {
				Ext_Func new_func = new_funcs.data[i];			
				
				gen_push_str(state, new_func.name);					
				gen_push_str(state, (String_View){output, strlen(output)});								
				Inst inst = create_inst(INST_LOAD_LIBRARY, (Word){.as_int=0}, 0);
				DA_APPEND(&state->machine.instructions, inst);
	            state->stack_s -= 2;
			}
			//gen_push_str(state, (String_View){output, strlen(output)});				
			//Inst inst = create_inst(INST_LOAD_LIBRARY, (Word){.as_int=0}, 0);
			//DA_APPEND(&state->machine.instructions, inst);
            //state->stack_s -= 2;
        } break;
        case BUILTIN_CALL: {
				/*		
            if(expr->value.builtin.value.count != 1) {
                PRINT_ERROR(expr->loc, "incorrect arg amounts for get");
            }
			for(size_t i = 0; i < expr->value.builtin.value.count; i++) {
				gen_expr(state, expr->value.builtin.value.data[i]);
			}
			*/
			Inst inst = create_inst(INST_NATIVE, (Word){.as_int=2}, 0);
			DA_APPEND(&state->machine.instructions, inst);
            //state->stack_s -= 2;
        } break;
    }
}

void gen_field_offset(Program_State *state, Struct structure, String_View var) {
    size_t offset = 0;
    size_t i;
    for(i = 0; !view_cmp(structure.values.data[i].value.var.name, var); i++) {
		Location loc = {0};
		loc.filename = "unknown";
        if(i == structure.values.count) PRINT_ERROR(loc, "unknown field: "View_Print" of struct: "View_Print, View_Arg(var), View_Arg(structure.name));
        offset += (1 * data_type_s[structure.values.data[i].value.var.type]);
    }
    gen_offset(state, offset);
    gen_push(state, data_type_s[structure.values.data[i].value.var.type]);
}
    
void gen_struct_field_offset(Program_State *state, String_View struct_name, String_View var) {
    Variable struct_var = get_variable(state, struct_name);
    Struct structure = get_struct(state->structs, struct_var.struct_name).value.structs;
    gen_indup(state, state->stack_s-struct_var.stack_pos);	
	gen_field_offset(state, structure, var);
}
    
void gen_expr(Program_State *state, Expr *expr) {
    switch(expr->type) {
        case EXPR_BIN:
            gen_expr(state, expr->value.bin.lhs);
            gen_expr(state, expr->value.bin.rhs);
			Inst inst = create_inst(op_types_inst[expr->value.bin.op.type], (Word){.as_int=0}, 0);
			DA_APPEND(&state->machine.instructions, inst);
            state->stack_s--;
            break;
        case EXPR_INT:
			switch(expr->data_type) {
				case TYPE_INT:
		            gen_push(state, expr->value.integer);        			
					break;
				case TYPE_U8:
				case TYPE_U16:
				case TYPE_U32:
				case TYPE_U64:
		            gen_push_u(state, expr->value.integer, type_to_data[expr->data_type]);        						
					break;
				default:
					ASSERT(false, "unreachable");
			}
            break;
        case EXPR_FLOAT:
            gen_push_float(state, expr->value.floating);        
            break;
        case EXPR_STR:
            gen_push_str(state, expr->value.string);
            break;
        case EXPR_CHAR:
            gen_push_char(state, expr->value.string);
            break;
        case EXPR_VAR: {
            int index = get_variable_location(state, expr->value.variable);
            if(index == -1) {
                PRINT_ERROR(expr->loc, "variable `"View_Print"` referenced before assignment", View_Arg(expr->value.variable));
            }
			if(get_variable(state, expr->value.variable).global) gen_global_indup(state, index);
            else gen_indup(state, state->stack_s-index); 
        } break;
		case EXPR_STRUCT: {
			size_t offset = 0;
			for(size_t i = 0; i < expr->value.structure.values.count; i++) {
				gen_structure_field(state, offset, expr->value.structure.values.data[i]);
				offset += data_type_s[expr->value.structure.values.data[i]->data_type];				
			}
		} break;
        case EXPR_FUNCALL: {
            Function *function = get_func(state->program.functions, expr->value.func_call.name);
            if(!function) { 
                PRINT_ERROR(expr->loc, "function `"View_Print"` referenced before assignment\n", View_Arg(expr->value.func_call.name));
            }
            if(function->args.count != expr->value.func_call.args.count) {
                PRINT_ERROR(expr->loc, "args count do not match for function `"View_Print"`\n", View_Arg(function->name));
            }
            for(size_t i = 0; i < expr->value.func_call.args.count; i++) {
                gen_expr(state, expr->value.func_call.args.data[i]);
            }
            gen_func_call(state, expr->value.func_call.name);
			for(size_t i = 0; i < expr->value.func_call.args.count; i++) {
				state->stack_s--;		
			}
			if(function->type != TYPE_VOID) state->stack_s++;
        } break;
        case EXPR_ARR: {
            int index = get_variable_location(state, expr->value.array.name);
            if(index == -1) {
                PRINT_ERROR(expr->loc, "variable `"View_Print"` referenced before assignment", View_Arg(expr->value.array.name));
            }
            Type_Type type = get_variable_type(state, expr->value.array.name);                        
            gen_arr_offset(state, index, expr->value.array.index, type);
            gen_push(state, data_type_s[type]);
            gen_read(state);
        } break;
        case EXPR_FIELD_ARR: {
            int index = get_variable_location(state, expr->value.array.name);
            if(index == -1) {
                PRINT_ERROR(expr->loc, "variable `"View_Print"` referenced before assignment", View_Arg(expr->value.array.name));
            }
            Type_Type type = get_variable_type(state, expr->value.array.name);                        
			Variable var = get_variable(state, expr->value.array.name);
			gen_arr_offset(state, index, expr->value.array.index, type);
            gen_push(state, data_type_s[type]);
            gen_read(state);
			Struct structure = get_struct(state->structs, var.struct_name).value.structs;
            String_View var_name = expr->value.array.var_name;			
			gen_field_offset(state, structure, var_name);			
            gen_read(state);            
        } break;
        case EXPR_FIELD: {
            String_View structure = expr->value.field.structure;
            String_View var_name = expr->value.field.var_name;
            gen_struct_field_offset(state, structure, var_name);
            gen_read(state);            
        } break;
		case EXPR_EXT: {
			String_View name = expr->value.ext.name;
			for(size_t i = 0; i < expr->value.ext.args.count; i++) {
				gen_expr(state, expr->value.ext.args.data[i]);
			}
			size_t ext_count = 0;			
			for(size_t i = 0; i < state->symbols.count; i++) {
				if(state->symbols.data[i].type == SYMBOL_EXT) {
					if(view_cmp(state->symbols.data[i].val.ext.name, name)) {
						gen_native(state, ext_count+2);
						state->stack_s -= expr->value.ext.args.count;
						if(expr->value.ext.return_type != TYPE_VOID) state->stack_s++;
						break;
					}
					ext_count++;					
				}
			}
			//ASSERT(false, "something went wrong with the external function "View_Print"'s args", View_Arg(name));
		} break;
        case EXPR_BUILTIN: {
            gen_builtin(state, expr);   
        } break;
        default:
            ASSERT(false, "UNREACHABLE, %d\n", expr->type);
    }       
}

void scope_end(Program_State *state) {
    ASSERT(state->scope_stack.count > 0, "scope stack count == 0");
    size_t target = state->scope_stack.data[state->scope_stack.count-1];
    while(state->stack_s > target) {
        gen_pop(state);
    }
    while(state->vars.count > 0 && state->vars.data[state->vars.count-1].stack_pos > state->stack_s) {
        state->vars.count--;
    }
}

void ret_scope_end(Program_State *state) {
    ASSERT(state->ret_stack.count > 0, "scope stack count == 0");
    size_t target = state->ret_stack.data[state->ret_stack.count-1];
    if(state->stack_s > 0) state->stack_s--;
    while(state->stack_s > target) {
        gen_pop(state);
    }
}
	
void gen_var_dec(Program_State *state, Node *node) {
       if(node->value.var.is_array && node->value.var.type != TYPE_STR) {
           gen_alloc(state, node->value.var.array_s, data_type_s[node->value.var.type]);
           for(size_t i = 0; i < node->value.var.value.count; i++) {
               gen_dup(state);
               gen_offset(state, data_type_s[node->value.var.type]*i);
               gen_expr(state, node->value.var.value.data[i]);                                                                                            
               gen_push(state, data_type_s[node->value.var.type]);
               gen_write(state);
           }
       } else if(node->value.var.is_struct) {
			Node cur_struct = get_struct(state->structs, node->value.var.struct_name);		
			for(size_t i = 0; i < node->value.var.value.data[0]->value.structure.values.count; i++) {
				node->value.var.value.data[0]->value.structure.name = cur_struct.value.structs.name;
			}
			size_t alloc_s = 0;
			for(size_t i = 0; i < cur_struct.value.structs.values.count; i++) {
				alloc_s += data_type_s[cur_struct.value.structs.values.data[i].value.var.type];
			}
			gen_struct_alloc(state, alloc_s);
			gen_expr(state, node->value.var.value.data[0]);
       } else {
           gen_expr(state, node->value.var.value.data[0]);                                    
       }
       node->value.var.stack_pos = state->stack_s;                 
       DA_APPEND(&state->vars, node->value.var);    
}
	
void gen_vars(Program_State *state, Program *program) {
	for(size_t i = 0; i < program->vars.count; i++) {
		Node *node = &program->vars.data[i];
		switch(node->type) {
            case TYPE_VAR_DEC: {
				gen_var_dec(state, node);
			    node->value.var.global = true;				
            } break;
			default: {
				ASSERT(false, "unexpected node");
			} break;
		}
	}
}

void gen_program(Program_State *state, Nodes nodes) {
    for(size_t i = 0; i < nodes.count; i++) {
        Node *node = &nodes.data[i];
        switch(node->type) {
            case TYPE_NATIVE:
                switch(node->value.native.type) {
                    case NATIVE_WRITE: {
                        if(node->value.native.args.count > 1) {
                            fprintf(stderr, "error: too many args\n");
                            exit(1);
                        }
                        gen_expr(state, node->value.native.args.data[0].value.expr);
                        gen_push(state, STDOUT);
						Inst inst = create_inst(INST_NATIVE, (Word){.as_int=node->value.native.type}, INT_TYPE);
						DA_APPEND(&state->machine.instructions, inst);
                        state->stack_s -= 2;
                    } break;
                    case NATIVE_EXIT: {
                        ASSERT(node->value.native.args.count == 1, "too many arguments");
                        if(node->value.native.args.data[0].type != ARG_EXPR) {
                            PRINT_ERROR(node->loc, "expected type int, but found type %s", node_types[node->value.native.args.data[0].type]);
                        };
                        gen_expr(state, node->value.native.args.data[0].value.expr);
						Inst inst = create_inst(INST_NATIVE, (Word){.as_int=node->value.native.type}, INT_TYPE);
						DA_APPEND(&state->machine.instructions, inst);
                        state->stack_s--;
                    } break;           
                    default:
                        ASSERT(false, "unreachable");
                }
                break;
            case TYPE_VAR_DEC: {
				gen_var_dec(state, node);
            } break;
            case TYPE_VAR_REASSIGN: {
				ASSERT(!node->value.var.is_const, "const variable cannot be reassigned");
                gen_expr(state, node->value.var.value.data[0]);
				Variable var = get_variable(state, node->value.var.name);
                //int index = get_variable_location(state, node->value.var.name);
				int index = var.stack_pos;
                if(index == -1) {
                    PRINT_ERROR(node->loc, "variable `"View_Print"` referenced before assignment", View_Arg(node->value.var.name));
                }
				if(var.global) gen_global_inswap(state, index);
                else gen_inswap(state, state->stack_s-index);    
				if(!var.is_struct) gen_pop(state);
            } break;
            case TYPE_FIELD_REASSIGN: {
                String_View structure = node->value.field.structure;
                String_View var_name = node->value.field.var_name;
                gen_struct_field_offset(state, structure, var_name);
                gen_expr(state, node->value.field.value.data[0]);
                gen_inswap(state, 1);
                gen_write(state);
				// TODO: MIGHT NEED a GEN_POP HERE
            } break;
            case TYPE_ARR_INDEX: {
                int index = get_variable_location(state, node->value.array.name);
                if(index == -1) {
                    PRINT_ERROR(node->loc, "array `"View_Print"` referenced before assignment", View_Arg(node->value.var.name));
                }
                Type_Type type = get_variable_type(state, node->value.array.name);                                            
                gen_arr_offset(state, index, node->value.array.index, type);
                gen_expr(state, node->value.array.value.data[0]);                                                    
                gen_push(state, data_type_s[type]);
                gen_write(state);
            } break;
            case TYPE_FUNC_DEC: {
                Function function = {0};
                memcpy(&function, &node->value.func_dec, sizeof(Function));
                DA_APPEND(&state->functions, function);
                DA_APPEND(&state->block_stack, BLOCK_FUNC);                
                DA_APPEND(&state->ret_stack, state->stack_s);                
                DA_APPEND(&state->scope_stack, state->stack_s);                
                for(size_t i = 0; i < function.args.count; i++) {
                    Variable var = {0};
                    var.stack_pos = ++state->stack_s;
                    var.name = function.args.data[i].value.var.name;
                    var.type = function.args.data[i].value.var.type;
                    DA_APPEND(&state->vars, var);    
                }
                gen_jmp(state, node->value.func_dec.label);                                
                gen_func_label(state, function.name);
            } break;
            case TYPE_FUNC_CALL: {
                Function *function = get_func(state->program.functions, node->value.func_call.name);
                if(!function) PRINT_ERROR(node->loc, "function `"View_Print"` referenced before assignment\n", View_Arg(node->value.func_call.name));
                if(function->args.count != node->value.func_call.args.count) {
                    PRINT_ERROR(node->loc, "args count do not match for function `"View_Print"`\n", View_Arg(function->name));
                }
                for(size_t i = 0; i < node->value.func_call.args.count; i++) {
                    gen_expr(state, node->value.func_call.args.data[i]);
                }
                gen_func_call(state, node->value.func_call.name);
                state->stack_s -= node->value.func_call.args.count;
                // for the return value
                if(function->type != TYPE_VOID) {
						gen_pop(state);
						state->stack_s++;
				}
            } break;
            case TYPE_RET: {
                if(state->functions.count == 0) {
                    PRINT_ERROR(node->loc, "return without function definition");
                }
                Function function = state->functions.data[state->functions.count-1];
                if(function.type == TYPE_VOID) {
                    PRINT_ERROR(node->loc, "function `"View_Print"` with return type of void returns value", View_Arg(function.name));    
                }
				// + 1 because we need to place it on the top of the stack after scope_end
                size_t pos = state->ret_stack.data[state->ret_stack.count-1] + 1;
                gen_expr(state, node->value.expr);
                ASSERT(pos <= state->stack_s, "pos is too great");
                gen_inswap(state, state->stack_s-pos);
                size_t pre_stack_s = state->stack_s;
                ret_scope_end(state);
                state->stack_s = pre_stack_s;
				Inst inst = create_inst(INST_RET, (Word){.as_int=0}, 0);
				DA_APPEND(&state->machine.instructions, inst);
            } break;
            case TYPE_IF: {
                DA_APPEND(&state->block_stack, BLOCK_IF);
                gen_expr(state, node->value.conditional);
            } break;
            case TYPE_ELSE: {
                ASSERT(state->block_stack.count > 0, "block stack underflowed");
                if(state->block_stack.data[--state->block_stack.count] == BLOCK_IF) {
                    gen_jmp(state, node->value.el.label2);
                } else {
                    PRINT_ERROR(node->loc, "expected `if` but found `%d`\n", state->block_stack.data[state->block_stack.count]);
                }
                DA_APPEND(&state->block_stack, BLOCK_ELSE);                
                gen_label(state, node->value.el.label1);
            } break;
            case TYPE_WHILE: {
                DA_APPEND(&state->block_stack, BLOCK_WHILE);
                DA_APPEND(&state->while_labels, state->while_label);
                gen_while_label(state, state->while_label++);
                gen_expr(state, node->value.conditional);
            } break;
            case TYPE_THEN: {
                gen_zjmp(state, node->value.label.num);            
				DA_APPEND(&state->scope_stack, state->stack_s);
            } break;
            case TYPE_END: {
                ASSERT(state->block_stack.count > 0, "block stack was underflowed");
                scope_end(state);                    
                state->scope_stack.count--;
                Block_Type block = state->block_stack.data[--state->block_stack.count];
                if(block == BLOCK_WHILE) {
                    gen_while_jmp(state, state->while_labels.data[--state->while_labels.count]);
                } else if(block == BLOCK_FUNC) {
					Inst inst = create_inst(INST_RET, (Word){.as_int=0}, 0);
					DA_APPEND(&state->machine.instructions, inst);
                }
                gen_label(state, node->value.label.num);
            } break;
            case TYPE_EXPR_STMT: {
                gen_expr(state, node->value.expr_stmt);
                if(node->value.expr_stmt->return_type != TYPE_VOID) gen_pop(state);
            } break;
            default:
                break;
        }    
    }

}
	
void gen_label_arr(Program_State *state) {
	Insts instructions = state->machine.instructions;
	for(size_t i = 0; i < instructions.count; i++) {
		switch(instructions.data[i].type) {
			case INST_JMP:
			case INST_ZJMP:
			case INST_NZJMP:
				if(instructions.data[i].data_type == PTR_TYPE) {
					instructions.data[i].data_type = INT_TYPE;
					break;
				}
				instructions.data[i].value.as_int = state->labels.data[instructions.data[i].value.as_int];			
				break;
			case INST_CALL:
				instructions.data[i].value.as_int = state->functions.data[instructions.data[i].value.as_int].label;						
				break;
			default:
				continue;
		}
	}
}
    
void generate(Program_State *state, Program *program) {
	for(size_t i = 0; i < program->ext_nodes.count; i++) {
		char *output = malloc(sizeof(char)*128);
		sprintf(output, View_Print".c", 
				View_Arg(program->ext_nodes.data[i].value.expr_stmt->value.builtin.ext_funcs.file_name));
		if(access(output, F_OK) != 0) {
			FILE *file = fopen(output, "w");
			fprintf(file, "#include <stdio.h>\n");
			fprintf(file, "#define TIM_IMPLEMENTATION\n");	
			fprintf(file, "#include <tim.h>\n");
			fprintf(file, "typedef void* pointer;\n");			
			fprintf(file, "typedef uint8_t u8;\n");					
			fprintf(file, "typedef uint16_t u16;\n");				
			fprintf(file, "typedef uint32_t u32;\n");				
			fprintf(file, "typedef uint64_t u64;\n");					
			for(size_t s = 0; s < state->structs.count; s++) {
				Struct cur_struct = state->structs.data[s].value.structs;
				fprintf(file, "typedef struct {\n");
				for(size_t f = 0; f < cur_struct.values.count; f++) {
					Variable arg = cur_struct.values.data[f].value.var;
					ASSERT(arg.type < DATA_COUNT, "type is invalid!");
					fprintf(file, "	%s "View_Print";\n", data_typess[arg.type], View_Arg(arg.name));
				}
				fprintf(file, "} native_"View_Print";\n", View_Arg(cur_struct.name));
			}
			fclose(file);
		}
		free(output);
	}

	for(size_t i = 0; i < program->ext_nodes.count; i++) {
		gen_builtin(state, program->ext_nodes.data[i].value.expr_stmt);
	}

	gen_vars(state, program);
    gen_program(state, program->nodes);
	gen_label_arr(state);	

	for(size_t i = 0; i < program->ext_nodes.count; i++) {
		char *output = malloc(sizeof(char)*128);
		sprintf(output, View_Print".c", 
				View_Arg(program->ext_nodes.data[i].value.expr_stmt->value.builtin.ext_funcs.file_name));
		if(access(output, F_OK) == 0) {
			ASSERT(remove(output) == 0, "Could not remove the native file %s: %s", output, strerror(errno));
		}
		free(output);
	}
}
