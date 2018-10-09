#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define fx_mask              0x03
#define fx_tag               0x00
#define fx_shift                2
#define bool_f               0x2F
#define bool_t               0x6F
#define bool_bit                6
#define list_nil             0x3F
#define char_mask            0x3F
#define char_tag             0x0F
#define char_shift              8
#define obj_mask             0x07
#define obj_shift               3
#define pair_tag             0x01
#define pair_size              16
#define pair_car                0
#define pair_cdr                8
#define vector_tag           0x05
#define string_tag           0x06
#define closure_tag          0x02


/* Schemeの値は、ptr型とする */
typedef uint32_t ptr;

typedef struct {
  ptr car;
  ptr cdr;
} cell;

typedef struct {
  ptr length;
  ptr buf[1];
} vector;

typedef struct {
  ptr length;
  char buf[1];
} string;

#define IN_LIST 1
#define OUT 0

int scheme_entry(void *heap);

static void print_ptr_rec(ptr x, int state)
{
  if ((x & fx_mask) == fx_tag) {
    printf("%d", (int32_t)x >> fx_shift);
  } else if (x == bool_f) {
    printf("#f");
  } else if (x == bool_t) {
    printf("#t");
  } else if (x == list_nil) {
    printf("()");
  } else if ((x & char_mask) == char_tag) {
    char c = (char)(x >> char_shift);
    if      (c == '\t') printf("#\\tab");
    else if (c == '\n') printf("#\\newline");
    else if (c == '\r') printf("#\\return");
    else if (c == ' ')  printf("#\\space");
    else                printf("#\\%c", c);
  } else if ((x & obj_mask) == pair_tag) {
    if (state != IN_LIST) printf("(");
    ptr car = ((cell*)(x - pair_tag))->car;
    print_ptr_rec(car, OUT);
    ptr cdr = ((cell*)(x - pair_tag))->cdr;
    if (cdr != list_nil) {
      if ((cdr & obj_mask) != pair_tag) {
	printf(" . ");
	print_ptr_rec(cdr, OUT);
      } else {
	printf(" ");
	print_ptr_rec(cdr, IN_LIST);
      }
    }
    if (state != IN_LIST) printf(")");
  } else if ((x & obj_mask) == vector_tag) {
    printf("#(");

    vector *p = (vector*)(x - vector_tag);
    unsigned long n = p->length >> fx_shift;
    unsigned i;
    for (i = 0; i < n; i++) {
      if (i > 0) printf(" ");
      print_ptr_rec(p->buf[i], OUT);
    }

    printf(")");
  } else if ((x & obj_mask) == string_tag) {
    printf("\"");

    string *p = (string*)(x - string_tag);
    unsigned long n = p->length >> fx_shift;
    unsigned long i;
    for (i = 0; i < n; i++) {
      int c = p->buf[i];
      if      (c == '"')  printf("\\\"");
      else if (c == '\\') printf("\\\\");
      else                putchar(c);
    }

    printf("\"");
  } else if ((x & obj_mask) == closure_tag) {
    printf("#<procedure>");
  } else {
    printf("#<unknown 0x%08x>", x);
  }
}

static void print_ptr(ptr x)
{
  print_ptr_rec(x, OUT);
  printf("\n");
}

int main(int argc, char *argv)
{
  extern void *heap;
  
  print_ptr(scheme_entry(heap));
  
  return 0;
}
