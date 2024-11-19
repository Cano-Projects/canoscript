#define ARENA_IMPLEMENTATION
#include "main.h"

//#define TIM_IMPLEMENTATION
//#define TIM_H
//#include "tim.h"

void usage(char *file) {
    fprintf(stderr, "usage: %s <option> <filename.cano>\n", file);
	fprintf(stderr, "options: com, run\n");
	fprintf(stderr, "show this menu: --help\n");
    exit(1);
}
    
void *custom_realloc(void *ptr, size_t size) {
    void *new_ptr = realloc(ptr, size);
    ASSERT(new_ptr != NULL, "Out of memory, maybe close some programs? Alternatively you could buy some more RAM.");
    ptr = new_ptr;
    return new_ptr;
}
	
char *shift(int *argc, char ***argv) {
		char *flag = *(*argv);
		*argc += 1;	
		*argv += 1;
		return flag;
}
	
int main(int argc, char **argv) {
	char *file = shift(&argc, &argv);
	char *flag = shift(&argc, &argv);
	char *filename = NULL;
	if(flag == NULL) usage(file);
    int compile = 0;
	if(strncmp(flag, "com", 3) == 0) {
		compile = 1;
		filename = shift(&argc, &argv);
	} else if(strncmp(flag, "run", 3) == 0) {
		compile = 0;
		filename = shift(&argc, &argv);		
	} else if(strncmp(flag, "db", 2) == 0) {
        compile = 2;
		filename = shift(&argc, &argv);		
    } else if(strncmp(flag, "dis", 3) == 0) {
        compile = 3;
		filename = shift(&argc, &argv);		        
    } else if(strncmp(flag, "--help", 6) == 0) {
		usage(file);
	} else {
		compile = false;
		filename = flag;
	}
	if(filename == NULL) usage(file);
	
	Arena token_arena = arena_init(sizeof(Token)*ARENA_INIT_SIZE);	
    String_View view = read_file_to_view(&token_arena, filename);
	Arena string_arena = arena_init(sizeof(char)*ARENA_INIT_SIZE);
    Token_Arr tokens = lex(&token_arena, &string_arena, filename, view);
	Arena node_arena = arena_init(sizeof(Node)*ARENA_INIT_SIZE);
    Program program = parse(&node_arena, tokens);
	
	arena_free(&token_arena);	
}
