#ifndef _FA_STACK_H
#define _FA_STACK_H
typedef void(*IT_FREE_FUNC)(void *ptr);
typedef void(*IT_ITER_FUNC)(void *ptr);
typedef struct stack_item {
  int freeme; // whether to free contents
  void *contents;
  struct stack_item *prev;
  struct stack_item *next;
  
} stack_item; // generic stack item struct

typedef struct stack {
  int size;
  IT_FREE_FUNC it_free;
  stack_item *top;
} stack; //generic stack struct

stack *stack_new(IT_FREE_FUNC it_free);
void stack_clear(stack *st);
void stack_free(stack *st);
void *stack_peek(stack *st, int index);
void *stack_pop(stack *st, int index);
void stack_iter(stack *st, IT_ITER_FUNC iter_func);
void stack_push(stack *st, void *item, int freeme);
#endif
