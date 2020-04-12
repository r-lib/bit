/*
# Fallback integer sorting
# (c) 2016 - 2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_sort
#define ALREADY_DEFINED_sort

#include "merge.h"

//IndexT randIndex(IndexT n);
void int_insertionsort(ValueT *x, IndexT l, IndexT r);
void int_quicksort2(ValueT *x, IndexT l, IndexT r);
void int_quicksort3(ValueT *x, IndexT l, IndexT r);

#endif
