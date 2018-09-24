#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>

#define bool_f	0x2F
#define bool_t	0x6F

#define fx_mask	0x03
#define fx_tag  0x00
#define fx_shift 2

#define empty_list 0x3F

#define char_mask 0xFF
#define char_tag  0x0F
#define char_shift 8

/* Schemeの値は、ptr型とする */
typedef uint32_t ptr;

int scheme_entry(void *heap);

static void print_ptr(ptr x)
{
  if ((x & fx_mask) == fx_tag) {
    printf("%d", (int32_t)x >> fx_shift);
  } else if (x == bool_f) {
    printf("#f");
  } else if (x == bool_t) {
    printf("#t");
  } else if (x == empty_list) {
    printf("()");
  } else if ((x & char_mask) == char_tag) {
    printf("%c", x >> char_shift);
  } else {
    printf("#<unknown 0x%08x>", x);
  }

  printf("\n");
}

int main(int argc, char *argv)
{
  extern void *heap;
  
  print_ptr(scheme_entry(heap));
  
  return 0;
}
