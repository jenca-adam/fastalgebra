#define PY_SSIZE_T_CLEAN
#include <Python.h>
#include <stddef.h>
#include "expr.h"

typedef struct Expression {
  PyObject_HEAD expr expression;
} Expression;

static PyObject *Expression_stringify(PyObject *self, PyObject *noargs) {
  return expr_stringify(&((Expression *)self)->expression, 1);
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
  static char *kwlist[] = {"string", "max_depth", NULL};
  int max_depth = 8192;

  if (!PyArg_ParseTupleAndKeywords(args, kwds, "s|i", kwlist, &str,
                                   &max_depth)) {
    return -1;
  }
  linked_list *toklist = expr_tokenize(str);
  if (!toklist) {
    return -1;
  }
  stack *globstack = stack_new(NULL);
  if (expr_parseinto(toklist, &self->expression, globstack, max_depth) == -1) {
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
static struct PyMethodDef Expression_methods[] = {
    {"stringify", Expression_stringify, METH_NOARGS,
     "converts the expression to a string"},
    {NULL, NULL, 0, NULL}};
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
