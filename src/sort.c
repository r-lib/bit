/*
# Fallback integer sorting
# (c) 2016 - 2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#include "merge.h"

#define RANDINDEX(funcnam) IndexT funcnam (IndexT n) \
{                                                    \
  IndexT r;                                          \
  while((r = ((IndexT)(unif_rand()*n))) >= n);       \
  return r;                                          \
}

static RANDINDEX(randIndex)
  

#define INSERTIONSORT(funcnam) void funcnam ( \
ValueT *x                                         \
  , IndexT l                                      \
  , IndexT r                                      \
)                                                 \
{                                                 \
  IndexT i;                                       \
  ValueT t;                                       \
  for (i=r;i>l;i--){                              \
    if (LT(x[i],x[i-1])) {                        \
      EXCH(x[i-1],x[i],t);                        \
    }                                             \
  }                                               \
  for (i=l+2;i<=r;i++){                           \
    IndexT j=i;                                   \
    ValueT v;                                     \
    MOVE(v, x[i])                                 \
      while (LT(v,x[j-1])){                       \
        MOVE(x[j], x[j-1])                        \
        j--;                                      \
      }                                           \
      MOVE(x[j], v)                               \
  }                                               \
}

INSERTIONSORT(int_insertionsort)
  
void int_quicksort2(ValueT *x, IndexT l, IndexT r)
  { 
#if INSERTIONSORT_LIMIT > 0
    if (r - l < INSERTIONSORT_LIMIT){
      int_insertionsort(x, l, r);
      return;
    }
#else    
    if (l >= r)
      return;
#endif  
    IndexT j, i = l+randIndex(r-l+1);
    ValueT t, v;
    EXCH(x[i], x[r], v); // first argument pivot now in v and x[r]
    i = l-1; j = r; 
    for (;;){
      while (LT(x[++i], v)); // sentinel stop of for loop
      while (LT(v, x[--j])) 
        if (j <= i)       // explicit stop of for loop
          break;
        if (j <= i) 
          break;
        EXCH(x[i], x[j], t);
    }
    EXCH(x[i], x[r], t);
    int_quicksort2(x, l, i-1);
    int_quicksort2(x, i+1, r);
  }


void int_quicksort3(ValueT *x, IndexT l, IndexT r)
{ 
#if INSERTIONSORT_LIMIT > 0
  if (r - l < INSERTIONSORT_LIMIT){
    int_insertionsort(x, l, r);
    return;
  }
#else    
  if (l >= r)
    return;
#endif  
  IndexT k, p, q, j, i = l+randIndex(r-l+1);
  ValueT t, v;
  EXCH(x[i], x[r], v); // first argument pivot now in v and x[r]
  i = p = l-1; j = q = r; 
  for (;;){
    while (LT(x[++i],v)); // sentinel stop of for loop
    while (LT(v,x[--j])) if (j <= i) break; // explicit stop of for loop
    if (j <= i) break;
    EXCH(x[i], x[j], t);
    if (EQ(x[i],v)) {++p; EXCH(x[p], x[i],t);}
    if (EQ(v,x[j])) {--q; EXCH(x[j], x[q], t);}
  }
  EXCH(x[i], x[r], t); 
  j = i-1; i = i+1;
  for (k = l; k < p; k++, j--) EXCH(x[k], x[j], t);
  for (k = r-1; k > q; k--, i++) EXCH(x[i], x[k], t);
  int_quicksort3(x, l, j);
  int_quicksort3(x, i, r);
}


void int_countsort(
    IndexT *x
  , IndexT *count
  , IndexT *range
  , IndexT l, IndexT r
)
{
  IndexT i, v, lim;
  IndexT nrange = range[1] - range[0] + 1;
  // initialize counter
  for (i=0;i<nrange;i++)
    count[i]=0;
  // count
  count -= range[0]; // move the pointer such that the first elements will hit pos 0 of the real vector (save -offset in each iteration)
  for (i=l;i<=r;i++)
    count[x[i]]++;
  count += range[0]; // move the pointer back
  // add sorted values to data
  for (i=0;i<nrange;i++){
    v = i + range[0];
    for (lim=l+count[i];l<lim;l++)
      x[l] = v;
  }
}


// ATTENTION: the following sorting algos assume that the input has been prepared using range_sortna, which implies
// - data vector x has been copied already hence we can do inplace (if not R's copying semantics are violated)
// - position and number of NAs are known (if wrong R might crash)
// - NAs are already in position which na_last dictates (if not R might crash)
// range_ is only needed for its third elements which contains the number of NAs
// NAs are kept outside the sort
// other range restrictions are *not* implemented 
//   -- range has nothing to do with the l,r positions
//   -- range is about values, not positions
//   -- the first two range elements are ignored

SEXP R_int_countsort(SEXP x_, SEXP range_, SEXP na_last_){
  int *x = INTEGER(x_);
  int n = LENGTH(x_);
  int *range = INTEGER(range_);
  int na_last = asLogical(na_last_);
  int *count;
  count   = (int *) R_alloc(range[1] - range[0] + 1, sizeof(int));
  if (na_last==FALSE){
    int_countsort(
        x
      , count
      , range
      , range[2], n-1    // skipping over number of NAs
    );
  }else{
    int_countsort(
        x
      , count
      , range
      , 0, n-1-range[2]  // limiting for number of NAs at the end
    );
  }  
  return x_;
}
SEXP R_int_quicksort2(SEXP x_, SEXP range_, SEXP na_last_){
  int *x = INTEGER(x_);
  int n = LENGTH(x_);
  int *range = INTEGER(range_);
  int na_last = asLogical(na_last_);
  GetRNGstate();
  if (na_last==FALSE){
    int_quicksort2((ValueT *)x, range[2], n-1);
  }else{
    int_quicksort2((ValueT *)x, 0, n-range[2]-1);
  }  
  PutRNGstate();
  return(x_);
}

SEXP R_int_quicksort3(SEXP x_, SEXP range_, SEXP na_last_){
  int *x = INTEGER(x_);
  int n = LENGTH(x_);
  int *range = INTEGER(range_);
  int na_last = asLogical(na_last_);
  GetRNGstate();
  if (na_last==FALSE){
    int_quicksort3((ValueT *)x, range[2], n-1);
  }else{
    int_quicksort3((ValueT *)x, 0, n-range[2]-1);
  }  
  PutRNGstate();
  return(x_);
}

