#include "stack.h"
#include <stdlib.h>
stack *stack_new(IT_FREE_FUNC it_free) {
  stack *st = malloc(sizeof(stack));
  st->size = 0;
  st->it_free = it_free;
  st->top = NULL;
  return st;
}
void _free_stack_item(stack_item *it, IT_FREE_FUNC it_free) {
  if (it->freeme && it_free) {
    it_free(it->contents);
  }
  free(it);
}
void stack_clear(stack *st) {
  stack_item *it = st->top;
  while (it) {
    stack_item *nit = it->prev;
    _free_stack_item(it, st->it_free);
    it = nit;
  }
  st->top = NULL; // to avoid accidental use of freed memory
  st->size = 0;
}
void stack_free(stack *st) {
  stack_clear(st);
  free(st);
}
void *stack_peek(stack *st, int index) {
  stack_item *it = st->top;
  while (it && index > 0) {
    index--;
    it = it->prev;
  }
  if (it) {
    return it->contents;
  }
  return NULL;
}

void *stack_pop(stack *st, int index) {
  stack_item *it = st->top;
  while (it && index > 0) {
    index--;
    it = it->prev;
  }

  if (it) {
    st->size--;
    void *contents = it->contents;
    stack_item *prev = it->prev;
    if(prev){
    	prev->next = it->next;
    }
    if (it->next) {
      it->next->prev = prev;
    } else if (it == st->top) {
      st->top = prev;
    }
    free(it);
    return contents;
  }
  return NULL;
}
void stack_iter(stack *st, IT_ITER_FUNC iter_func) {
  stack_item *it = st->top;
  while (it) {
    iter_func(it->contents);
    it = it->prev;
  }
}
void stack_push(stack *st, void *item, int freeme) {
  stack_item *it = malloc(sizeof(stack_item));
  if (st->top) {
    st->top->next = it;
  }
  it->freeme = freeme;
  it->contents = item;
  it->prev = st->top;
  it->next = NULL;
  st->top = it;
  st->size++;
}
