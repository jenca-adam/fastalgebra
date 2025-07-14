#include "ll.h"
#include "stack.h"
#include <Python.h>
static const int add_sub_priority = 1;
static const int mul_div_priority = 2;
static const int impl_mul_priority = 3;
static const int pow_priority = 5;
typedef enum {
  CONST = 0, // constants
  ADD = 1,   // addition operation
  MUL = 2,   // multiplication
  DIV = 3,   // division
  IMPL_MUL = 4, // implicit multiplication, only used to specify a multiplication with a higher priority
  POW = 4,   // power
  VAR = 5,   // variable with a coefficient
  NONE = -1, // uninitialised/invalid

} expr_type;

typedef enum {
  NUMBER = 0,
  VARIABLE = 1,
  OP_ADD = 3,
  OP_SUB = 4,
  OP_MUL = 5,
  OP_DIV = 6,
  OP_POW = 7,
  PAREN_L = 8,
  PAREN_R = 9,
  INVALID = -1
} token_type;

typedef struct parser_token {
  token_type type;
  char *spelling;
  double value;
  int index;
} parser_token;

typedef struct expr {
  int num_children;
  int num_arguments;
  struct expr **children;
  expr_type type;
  signed char sign; // -1: negative, 1: positive, 0: zero
  double *arguments;
  char varname;
  int depth;
} expr;
PyObject *expr_stringify(expr *xp, int add_sign);
int expr_type_priority(expr_type et);
expr_type op_type(token_type op);
int is_unary(expr_type et);
signed char get_sign(stack *globstack);
int alpha_scan(const char *str, char *target, int limit);
void free_token(parser_token *token);
void print_token(parser_token *token);
void print_expr(expr *expr);
double expr_evaluate(expr *xp, PyObject *d);
PyObject *expr_stringify_child(expr *chld, int priority, int can_associate);
PyObject *expr_stringify(expr *xp, int add_sign);
linked_list *expr_tokenize(char *str);
void expr_free(expr *expression, int free_root);
void expr_free_root(expr *expression);
int expr_parseinto(linked_list *toklist, expr *expression, stack *globstack,
                   int max_depth);



