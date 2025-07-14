#include "ll.h"
#include <stdlib.h>
linked_list *ll_new(IT_FREE_FUNC it_free) {
  linked_list *ll = malloc(sizeof(linked_list));
  ll->size = 0;
  ll->it_free = it_free;
  ll->top = NULL;
  ll->root = NULL;
  return ll;
}

void ll_clear(linked_list *ll) {
  stack_clear((stack *)ll); // reuse stack functions, since a linked list is
                            // just a stack with a root
  ll->root = NULL;
}
void ll_free(linked_list *ll) {
  ll_clear(ll);
  free(ll);
}
void *ll_peek(linked_list *ll, int index) {
  return stack_peek((stack *)ll, index);
}

void *ll_pop(linked_list *ll, int index) {
  return stack_pop((stack *)ll, index);
}

void ll_push(linked_list *ll, void *item, int freeme) {
  stack_push((stack *)ll, item, freeme);
  if (!ll->root) {
    ll->root = ll->top;
  }
}

void *ll_get(linked_list *ll, int index) {
  ll_item *it = ll->root;
  while (it && index > 0) {
    index--;
    it = it->next;
  }
  if (it) {
    return it->contents;
  }
  return NULL;
}
void ll_iter(linked_list *ll, IT_ITER_FUNC iter_func) {
  ll_item *it = ll->root;
  while (it) {
    iter_func(it->contents);
    it = it->next;
  }
}
int ll_insert(linked_list *ll, int index, void *item, int freeme) {
  // for adding on top, use ll_push instead
  if (index == ll->size) {
    ll_push(ll, item, freeme);
    return 0;
  }
  ll_item *it = ll->root;
  while (it && index > 0) {
    index--;
    it = it->next;
  }
  if (it) {
    ll_item *nit = malloc(sizeof(ll_item));
    nit->freeme = freeme;
    nit->contents = item;
    nit->prev = it->prev;
    nit->next = it;
    it->prev = nit;
    ll->size++;
    return 0;
  }
  return -1;
}
