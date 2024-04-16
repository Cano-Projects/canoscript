#ifndef TIPP_H
#define TIPP_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#include "hashmap.h"
#include "view.h"

String_View read_file_to_buff(char *file_name);
String_View get_word(String_View *view);
String_View get_filename(String_View *view);
String_View get_value(String_View *view);
void eof_error(char *buffer, int index, int length);
String_View prepro(char *file_name, int depth);
void append_to_output(char *output, int *output_index, char *value, int value_length);
String_View pass(String_View view, int depth, char *file_name);

#ifndef COMMENT_CHAR
#define COMMENT_CHAR ';'
#endif

#ifndef DA_APPEND
#define DA_APPEND
#ifndef ASSERT
#define ASSERT
#define ASSERT(cond, ...) \
    do { \
        if (!(cond)) { \
            fprintf(stderr, "%s:%d: ASSERTION FAILED: ", __FILE__, __LINE__); \
            fprintf(stderr, __VA_ARGS__); \
            fprintf(stderr, "\n"); \
            exit(1); \
        } \
    } while (0)
#endif // ASSERT
	
#define DA_APPEND(da, item) do {                                                       \
    if ((da)->count >= (da)->capacity) {                                               \
        (da)->capacity = (da)->capacity == 0 ? DATA_START_CAPACITY : (da)->capacity*2; \
        (da)->data = custom_realloc((da)->data, (da)->capacity*sizeof(*(da)->data));       \
        ASSERT((da)->data != NULL, "outta ram");                               \
    }                                                                                  \
    (da)->data[(da)->count++] = (item);                                               \
} while (0)
#endif // DA_APPEND

#endif // TIPP_H

#ifdef TIPP_IMPLEMENTATION

const unsigned initial_size = 1;
struct hashmap_s hashmap;
int hashmap_initted = 0;

String_View read_file_to_buff(char *file_name){
    FILE *file = fopen(file_name, "r"); 
    if(file == NULL){
        fprintf(stderr, "error: file not found: %s\n", file_name);
        exit(1);
    }

    fseek(file, 0, SEEK_END);
    size_t len = ftell(file);
    fseek(file, 0, SEEK_SET);

    char *current = malloc(sizeof(char) * len);
    fread(current, sizeof(char), len, file);
    if(current == NULL){
        fprintf(stderr, "error: could not read from file: %s\n", file_name);
        fclose(file);
        exit(1);
    }

    fclose(file);
    return view_create(current, len);
}

String_View get_word(String_View *view){
	String_View start = *view;
    while(view->len > 0 && (isalpha(*view->data) || *view->data == '_')){
		*view = view_chop_left(*view);
    }
	return (String_View) {
		.data = start.data,
		.len = view->data-start.data,
	};
} 

String_View get_filename(String_View *view) {
	String_View start = *view;
    while(view->len > 0 && (isalpha(*view->data) || *view->data == '.' || isdigit(*view->data))){
		*view = view_chop_left(*view);
    }
	return (String_View) {
		.data = start.data,
		.len = view->data-start.data,
	};
	
} 

String_View get_value(String_View *view) {
	String_View start = *view;
    while(view->len > 0 && *view->data != '\n'){
		*view = view_chop_left(*view);
    }
	return (String_View) {
		.data = start.data,
		.len = view->data-start.data,
	};
	
} 

void eof_error(char *buffer, int index, int length){
    if(buffer[index] == '\0' || buffer == NULL || index > length){
        fprintf(stderr, "error: reached end of file\n");
        exit(1);
    }
}

String_View prepro(char *file_name, int depth){
    if(!hashmap_initted){
        int hashmap_error = hashmap_create(initial_size, &hashmap);
        assert(hashmap_error == 0 && "COULD NOT INITIALIZE HASHMAP\n");
        hashmap_initted = 1;
    }
    String_View buffer = read_file_to_buff(file_name);
	assert(buffer.data != NULL && "There was an issue with the buffer\n");
    String_View result = pass(buffer, depth, file_name);

    return result;
}

void append_to_output(char *output, int *output_index, char *value, int value_length){
    for(int i = 0; i < value_length; i++){
        output[*output_index] = value[i];
        *output_index += 1;
    }
}
	
typedef struct {
	char *data;
	size_t count;
	size_t capacity;
} Tipp_Str;
	
void append_file_info(Tipp_Str *output, String_View filename, size_t line) {
	DA_APPEND(output, '@');
	DA_APPEND(output, '"');				
	for(size_t i = 0; i < filename.len; i++) {
		DA_APPEND(output, filename.data[i]);
	}
	DA_APPEND(output, '"');				
	DA_APPEND(output, ' ');
	char *line_nums = malloc(sizeof(char)*64);
	sprintf(line_nums, "%zu", line);
	for(size_t i = 0; i < strlen(line_nums); i++) {
			DA_APPEND(output, line_nums[i]);
	}
	DA_APPEND(output, '\n');
	free(line_nums);
}

String_View pass(String_View view, int depth, char *file_name){
	(void)file_name;
    if(depth > 500){
        fprintf(stderr, "error: recursive import detected\n");
        exit(1);
    }
    int index = 0; 
    int line = 1;
	Tipp_Str output = {0};
    while(view.len > 0) {
        if(*view.data == '\n'){
            line++;
        } else if(*view.data == COMMENT_CHAR){
            while(view.len > 0 && *view.data != '\n'){
				view = view_chop_left(view);
            }
            line++;
        } else if(*view.data == '@'){
			view = view_chop_left(view);
            String_View word = get_word(&view);
            if(view_cmp(word, LITERAL_CREATE("def"))){
				view = view_chop_left(view);
                String_View def = get_word(&view);
				view = view_chop_left(view);
                String_View value = get_value(&view);
                line++;
                int put_error = hashmap_put(&hashmap, def.data, def.len, (void* const)value.data);
				// TODO: error instead of assert
                assert(put_error == 0 && "COULD NOT PLACE INTO HASHMAP\n");
            } else if(view_cmp(word, LITERAL_CREATE("imp"))){
				view = view_chop_left(view);
                if(*view.data != '"'){
                    fprintf(stderr, "error: expected open quote\n");
                    exit(1);
                }
				view = view_chop_left(view);
                String_View imported_file = get_filename(&view);
                if(*view.data != '"'){
                    fprintf(stderr, "error: expected close quote\n");
                    exit(1);
                }
				view = view_chop_left(view);
				char *imported_name = view_to_cstr(imported_file);
				strncpy(imported_name, imported_file.data, imported_file.len);
                String_View imported_buffer = prepro(imported_name, depth + 1);
				append_file_info(&output, imported_file, 1);
				for(size_t i = 0; i < imported_buffer.len; i++) {
					DA_APPEND(&output, imported_buffer.data[i]);
				}
				free(imported_name);									
				append_file_info(&output, view_create(file_name, strlen(file_name)), line);				
                //char *file_info = malloc(sizeof(char) * 64);
                //sprintf(file_info, "\n@\"%s\" %d\n", imported_file, 1);
                //append_to_output(output, &output_index, file_info, strlen(file_info));
                line++;
				view = view_chop_left(view);
				continue;
            } else {
                fprintf(stderr, "Unexpected keyword: "View_Print"\n", View_Arg(word));
                exit(1);
            }
        } else if(isalpha(*view.data)){
            // set a temp index because we dont want to iterate index here
            int temp_index = index;
            String_View word = get_word(&view);
            char* const element = hashmap_get(&hashmap, word.data, word.len);
            if(element){
                // if found in hashmap, set the element value instead of word value
                for(size_t i = 0; i < strlen(element); i++){
					DA_APPEND(&output, element[i]);
                }
                index = temp_index;
            } else {
                // if not found in hashmap, set the word value instead of element value
                for(size_t i = 0; i < word.len; i++){
					DA_APPEND(&output, word.data[i]);
                }
                index = temp_index;
            }
        } 

		DA_APPEND(&output, *view.data);
		view = view_chop_left(view);
    } 
    return view_create(output.data, output.count);
}

#endif
