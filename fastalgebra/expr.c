#define PY_SSIZE_T_CLEAN
#include "ll.h"
#include "stack.h"
#include <Python.h>
#include <stddef.h>
#include <stdio.h>
/* these should really be in a header file but who cares */
typedef enum {
  CONST = 0,     // constants
  ADD = 1,       // addition operation
  MUL = 2,       // multiplication
  DIV = 3,       // division
  POW = 4,       // powet
  VAR_COEFF = 5, // variable with a coefficient
  NONE = -1,     // uninitialised/invalid

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
} parser_token;

typedef struct expr {
  int num_children;
  int num_arguments;
  struct expr **children;
  expr_type type;
  signed char sign; // -1: negative, 1: positive, 0: zero
  double *arguments;
  char varname;
} expr;

typedef struct Expression {
  PyObject_HEAD expr expression;
} Expression;

int expr_type_priority(expr_type et) {
  if (et == ADD) {
    return 1;
  }
  if (et == MUL || et == DIV) {
    return 2;
  }
  if (et == POW) {
    return 3;
  }
  return -1;
}
expr_type op_type(token_type op) {
  if (op == OP_ADD || op == OP_SUB) {
    return ADD;
  }
  if (op == OP_MUL) {
    return MUL;
  }
  if (op == OP_DIV) {
    return DIV;
  }
  if (op == OP_POW) {
    return POW;
  }
  return NONE;
}
signed char get_sign(stack *globstack) {
  if (!globstack->top) {
    return 1;
  }
  stack_item *it = globstack->top->prev;
  signed char sign = 1;
  while (it) {
    token_type type = ((parser_token *)(it->contents))->type;
    if (type == OP_SUB) {
      sign = -sign;
    } else if (type != OP_ADD) {
      break;
    }
    it = it->prev;
  }
  return sign;
}
int alpha_scan(const char *str, char *target, int limit) {
  if (!str || !target || limit <= 0)
    return 0;

  int bytes_read = 0;
  int end = 0;
  while (isalpha((unsigned char)str[bytes_read])) {
    if (bytes_read < limit - 1) {
      target[bytes_read] = str[bytes_read];
      end++;
    }
    bytes_read++;
  }
  target[end] = '\0';

  return bytes_read;
}

void free_token(parser_token *token) {
  free(token->spelling);
  free(token);
}

// Function to print a single token
void print_token(parser_token *token) {
  printf("------------\n");
  if (!token) {
    printf("NULL token\n");
    return;
  }

  printf("Type: %d\n", token->type);
  printf("Spelling: %s\n", token->spelling ? token->spelling : "NULL");
  printf("Value: %lf\n", token->value);
}

void _print_expr(expr *expr, int indent) {
  if (!expr) {
    printf("%*sNULL expression\n", indent, "");
    printf("%*s---------------\n", indent, "");
    return;
  }
  printf("%*sType: %d\n", indent, "", expr->type);
  printf("%*sSign: %d\n", indent, "", expr->sign);
  if (expr->varname) {
    printf("%*sVariable name: %c\n", indent, "", expr->varname);
  }
  if (expr->num_arguments > 0) {
    printf("%*sArguments: ", indent, "");
    for (int i = 0; i < expr->num_arguments; i++) {
      printf(" %lf", expr->arguments[i]);
    }
    printf("\n");
  }
  if (expr->num_children > 0) {
    printf("%*sChildren:\n", indent, "");

    printf("%*s---------------\n", indent + 4, "");
    for (int i = 0; i < expr->num_children; i++) {
      _print_expr(expr->children[i], indent + 4);
    }
  }
  printf("%*s---------------\n", indent, "");
}
void print_expr(expr *expr) { _print_expr(expr, 0); }
linked_list *expr_tokenize(char *str) {
  int bytes_read;
  char *origin = str;
  linked_list *tokens = ll_new((IT_FREE_FUNC)free_token);
  while (*str) { // while not at end of string
    // strip whitespace
    while (isspace((unsigned char)*str))
      str++;

    printf("CHAR: %c\n", *str);
    if (*str == '\0') {
      break;
    } else if (isdigit(*str) || *str == '.') { // omit zero ok
      double value;
      if (sscanf(str, "%lf%n", &value, &bytes_read)) {
        parser_token *ntok = malloc(sizeof(parser_token));
        ntok->type = NUMBER;
        ntok->spelling = malloc(sizeof(char) * bytes_read + 1); // null
        ntok->value = value;
        memcpy(ntok->spelling, str, bytes_read); // this is ok
        ntok->spelling[bytes_read] = '\0';       // as long as we do this
        ll_push(tokens, ntok, 1);
      }
      str += bytes_read;
    } else {
      token_type toktype = INVALID;
      char *spelling = NULL;
      switch (*str) {
      case '(':
        toktype = PAREN_L;
        spelling = strdup("(");
        str++;
        break;
      case ')':
        toktype = PAREN_R;
        spelling = strdup(")");
        str++;
        break;
      case '+':
        toktype = OP_ADD;
        spelling = strdup("+");
        str++;
        break;
      case '-':
        toktype = OP_SUB;
        spelling = strdup("-");
        str++;
        break;
      case '*':
        toktype = OP_MUL;
        spelling = strdup("*");
        str++;
        break;
      case '/':
        toktype = OP_MUL;
        spelling = strdup("/");
        str++;
        break;
      case '^':
        toktype = OP_POW;
        spelling = strdup("^");
        str++;
        break;
      default:
        if (!isalpha((unsigned char)*str)) {
          ll_free(tokens);
          PyErr_Format(PyExc_ValueError,
                       "Invalid character in expression at index %d: '%c'",
                       (str - origin), *str);
          return NULL;
        }
        char varname;
        if (sscanf(str, "%c%n", &varname, &bytes_read)) {
          toktype = VARIABLE;
          spelling = malloc(2 * sizeof(char));
          spelling[0] = varname;
          spelling[1] = '\0';
          str += bytes_read;
        }
        break;
      }
      printf("TOKTYPE %d\n", toktype);
      if (toktype != INVALID) {
        parser_token *ntok = malloc(sizeof(parser_token));
        ntok->type = toktype;
        ntok->spelling = spelling;
        ntok->value = 0;
        ll_push(tokens, ntok, 1);
      }
    }
  }
  return tokens;
}

void expr_free(expr *expression, int free_root) {
  if (!expression) {
    return;
  }
  if (expression->arguments) {
    free(expression->arguments);
  }
  for (int i = 0; i < expression->num_children; i++) {
    expr_free(expression->children[i], 1);
  }
  free(expression->children);
  if (free_root) {
    free(expression);
  }
}
void expr_free_root(expr *expression) { expr_free(expression, 1); }
expr *expr_new(int num_children, int num_arguments, expr_type type,
               signed char sign, char varname) {
  expr *expression = malloc(sizeof(expr));
  expression->num_children = num_children;
  expression->num_arguments = num_arguments;
  expression->children = calloc(num_children, sizeof(expr *));
  expression->arguments = malloc(num_arguments * sizeof(double));
  expression->type = type;
  expression->sign = sign;
  expression->varname = varname;
  return expression;
}
int expr_parseinto(linked_list *toklist, expr *expression, stack *globstack) {
  if (!toklist)
    return -1;
  ll_iter(toklist, (IT_ITER_FUNC)print_token);
  stack *opstack = stack_new(NULL);
  stack *valstack = stack_new((IT_FREE_FUNC)expr_free);
  expr *last = NULL;
  ll_item *it = toklist->root; // this eventually gets consumed
  while (it) {
    expr *nexpr;
    stack_push(globstack, it->contents, 0);
    parser_token *tok = it->contents;
    switch (tok->type) {
    case NUMBER:
      nexpr = expr_new(0, 1, CONST, 1, '\0');
      nexpr->arguments[0] = tok->value;
      nexpr->sign = get_sign(globstack);
      stack_push(valstack, nexpr, 0);
      break;
    case VARIABLE:
      nexpr = expr_new(0, 0, VAR_COEFF, 1, tok->spelling[0]);
      // handle implicit multiplication
      if (globstack->top->prev &&
          (((parser_token *)(globstack->top->prev->contents))->type ==
               VARIABLE ||
           ((parser_token *)(globstack->top->prev->contents))->type == CONST ||
           ((parser_token *)(globstack->top->prev->contents))->type ==
               PAREN_R) && // man we could really use some macros
          valstack->top) { // if the last item is not an operation,
                           // nothing's going to modify the stack, so the last
                           // value stays on top
        expr *last = (expr *)valstack->top->contents;
        expr *implicit_mul = expr_new(2, 0, MUL, last->sign, '\0'); // copy sign
        last->sign = 1; // we copied the sign, so change this to positive so we
                        // don't cancel it out
        implicit_mul->children[0] = last;
        implicit_mul->children[1] = nexpr;
        valstack->top->contents = implicit_mul; // no need to pop if we're just
                                                // going to push back again
      } else {
        nexpr->sign = get_sign(globstack);
        stack_push(valstack, nexpr, 0);
      }
      break;
    case PAREN_R:
      toklist->root = it;
      goto end;
      break;
    case PAREN_L:
      nexpr = malloc(sizeof(expr));
      signed char sign =
          get_sign(globstack);  // get sign now since we modify the global stack
      toklist->root = it->next; // start evaluating from the next token
      int ok = expr_parseinto(toklist, nexpr, globstack);
      it = toklist->root;
      if (ok == -1) {
        free(nexpr);
        goto error;
      }
      nexpr->sign *= sign; // trick
      stack_push(valstack, nexpr, 0);
      if (!it) {
        goto end;
      }
      break;
    default:; // -pedantic go brrr
      expr_type type = op_type(tok->type);
      if (type == NONE) {
        break;
      }
      int new_priority = expr_type_priority(type);
      // while the next operation has lower priority than the previous
      while (opstack->top &&
             expr_type_priority(((expr *)(opstack->top->contents))->type) >=
                 new_priority) {

        expr *op = stack_pop(opstack, 0);
        if (valstack->size >= 2) {
          op->children[1] = stack_pop(valstack, 0);
          op->children[0] = stack_pop(valstack, 0);
          stack_push(valstack, op, 0);
        } else { // unary minus already gets handled by get_sign, so just
                 // discard the op if there's not enough args
          expr_free(
              op,
              1); // don't exit the loop here, we want to free unused unary ops
        }
      }
      nexpr = expr_new(2, 0, type, 1, '\0');
      stack_push(opstack, nexpr, 0);
      break;
    }
    it = it->next;
  };
end:
  while (opstack->top) {
    expr *op = stack_pop(opstack, 0);
    if (valstack->size >= 2) {
      op->children[1] = stack_pop(valstack, 0);
      op->children[0] = stack_pop(valstack, 0);
      stack_push(valstack, op, 0);
    } else {
      expr_free(op, 1);
    }
  }
  printf("GLOBAL STACK CONTENTS:\n");
  stack_iter(globstack, (IT_ITER_FUNC)print_token);
  printf("OPERATION STACK CONTENTS:\n");
  stack_iter(opstack, (IT_ITER_FUNC)print_expr);
  printf("VALUE STACK CONTENTS:\n");
  stack_iter(valstack, (IT_ITER_FUNC)print_expr);

  if (valstack->size > 1) {
    PyErr_SetString(PyExc_ValueError, "excess items in expression");
    goto error;
  }
  if (valstack->top && valstack->top->contents) {
    *expression = *(expr *)valstack->top->contents;
    free(valstack->top->contents);
  } else {
    PyErr_SetString(PyExc_ValueError, "nothing in value stack");
    goto error;
  }
  stack_free(valstack);
  stack_free(opstack);
  return 0;
error:
  stack_free_all(valstack);
  stack_free(opstack);

  return -1;
}

static void Expression_dealloc(Expression *self) {
  if (self) {
    expr_free(&self->expression, 0);
    Py_TYPE(self)->tp_free((PyObject *)self);
  }
}

static PyObject *Expression_new(PyTypeObject *type, PyObject *args,
                                PyObject *kwds) {
  Expression *self = (Expression *)type->tp_alloc(type, 0);
  if (self) {
    expr expression = self->expression;
    expression.num_children = 0;
    expression.num_arguments = 0;
    expression.children = NULL;
    expression.type = -1;
    expression.sign = 0;
    expression.arguments = NULL;
    expression.varname = 0;
  }
  return (PyObject *)self;
}

static int Expression_init(Expression *self, PyObject *args, PyObject *kwds) {
  char *str;
  if (!PyArg_ParseTuple(args, "s", &str)) {
    return -1;
  }
  linked_list *toklist = expr_tokenize(str);
  if (!toklist) {
    return -1;
  }
  stack *globstack = stack_new(NULL);
  if (expr_parseinto(toklist, &self->expression, globstack) == -1) {
    return -1;
  }
  stack_free(globstack);
  ll_free(toklist); // we created it, we free it.
                    // even if the root gets shifted this is ok, since ll_free
                    // frees from the top
}

static struct PyMemberDef Expression_members[] = {
    {"num_children", Py_T_INT,
     offsetof(Expression, expression) + offsetof(expr, num_children)},
    {"num_arguments", Py_T_INT,

     offsetof(Expression, expression) + offsetof(expr, num_arguments)},
    {"type", Py_T_INT, offsetof(Expression, expression) + offsetof(expr, type)},
    {"sign", Py_T_BYTE,
     offsetof(Expression, expression) + offsetof(expr, sign)},
    {"varname", Py_T_CHAR,
     offsetof(Expression, expression) + offsetof(expr, varname)},
    {NULL}};
static struct PyMethodDef Expression_methods[] = {{NULL, NULL, 0, NULL}};
static PyTypeObject ExpressionType = {
    PyVarObject_HEAD_INIT(NULL, 0).tp_name = "fastalgebra.expr.Expression",
    .tp_doc = "A basic Expression class",
    .tp_basicsize = sizeof(Expression),
    .tp_itemsize = 0,
    .tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE,
    .tp_members = Expression_members,
    .tp_methods = Expression_methods,
    .tp_new = Expression_new,
    .tp_dealloc = (destructor)Expression_dealloc,
    .tp_init = (initproc)Expression_init};
static PyMethodDef expr_methods[] = {{NULL, NULL, 0, NULL}};
static struct PyModuleDef exprmodule = {
    PyModuleDef_HEAD_INIT, "fastalgebra.expr", NULL, -1, expr_methods};
PyMODINIT_FUNC PyInit_expr(void) {
  if (PyType_Ready(&ExpressionType) < 0) {
    return NULL;
  }
  PyObject *m = PyModule_Create(&exprmodule);
  Py_INCREF(&ExpressionType);
  if (PyModule_AddObject(m, "Expression", (PyObject *)&ExpressionType) < 0) {
    Py_DECREF(&ExpressionType);
    Py_DECREF(m);
    return NULL;
  }
  return m;
}
