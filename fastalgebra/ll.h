#ifndef _FA_LL_H
#define _FA_LL_H
#include"stack.h"
typedef stack_item ll_item;
typedef struct linked_list{
	int size;
	IT_FREE_FUNC it_free;
	ll_item *top;
	ll_item *root;
} linked_list;

linked_list *ll_new(IT_FREE_FUNC it_free);
void ll_clear(linked_list *ll);
void ll_free(linked_list *ll);
void *ll_peek(linked_list *ll, int index);
void *ll_pop(linked_list *ll, int index);
void ll_push(linked_list *ll, void *item, int freeme);
void *ll_get(linked_list *ll, int index);
void ll_iter(linked_list *ll, IT_ITER_FUNC iter_func);
int ll_insert(linked_list *ll, int index, void *item, int freeme);
#endif
