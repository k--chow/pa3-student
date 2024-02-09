#include <inttypes.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define TRUE 0x0000000000000002L
#define FALSE 0x0000000000000000L

#define NOT_INT_ERROR 299
#define NOT_BOOL_ERROR 99
#define OVERFLOW_ERROR 399

#define BOA_MIN (- (1L << 62))
#define BOA_MAX ((1L << 62) - 1)

extern int64_t our_code_starts_here(int64_t input_val) asm("our_code_starts_here");
extern void error(int64_t error_code, int64_t val) asm("error");

void print(int64_t val) {
  int64_t new_val = (val-1)/2;
  if (((new_val <= BOA_MAX) && (new_val >= BOA_MIN)) && (val & 1)) {
    printf("%ld\n", new_val);
  }
  else if (val == TRUE) {
    printf("true\n");
  }
  else if (val == FALSE) {
    printf("false\n");
  }
  else {
    printf("Unrepresentable error\n");
  }
  //printf("raw %ld\n", val);
}

void error(int64_t error_code, int64_t val) {
  if (error_code == NOT_BOOL_ERROR) {
    fprintf(stderr, "Expected boolean, but got %ld\n", val);  
  } else if (error_code == NOT_INT_ERROR) {
    fprintf(stderr, "expected a number, but got %ld\n", val); 
  } else if (error_code == OVERFLOW_ERROR) {
    fprintf(stderr, "Overflow %ld\n", val);  
  } else {
    fprintf(stderr, "Nah error code: %ld val %ld\n", error_code, val);
    print(val);
    print(error_code);
  }
  exit(error_code);
}

int main(int argc, char** argv) {
  int64_t input_val;
 
  if (argc == 2) {
    char *argument = argv[1];
    char *endptr;
    errno = 0;

    input_val = strtoll(argument, &endptr, 10);
    
    if ((*endptr == '\0' && endptr != argument)) {
      // check its within min
      if ((*endptr == '\0' && endptr != argument) && (input_val < BOA_MIN || input_val > BOA_MAX)) {
        fprintf(stderr, "input is not a representable number\n");
        exit(1);
      } else if (*endptr == '\0' && endptr != argument) {
        //printf("boom");
        input_val = (input_val * 2) + 1; //unified runtime representation, since put on stack without preprocessing?
      } 
    } else {
      // gets put out here
        fprintf(stderr, "input must be a number\n");
        exit(1);
    }
    printf("Command-line argument: %s\n", argument);
    printf("After processing: %ld\n", input_val);
  }
  int64_t val = our_code_starts_here(input_val);
  print(val);
  return 0;
}
 
