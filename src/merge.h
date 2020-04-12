/*
# Fast methods for sorted integers
# (c) 2016 - 2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#ifndef ALREADY_DEFINED_merge
#define ALREADY_DEFINED_merge

#include <R.h>
//#include <Rdefines.h>
#include <Rinternals.h>

#undef DEBUG
#define NDEBUG 1
// #define DEBUG
// #undef NDEBUG

typedef int IndexT;
typedef int ValueT;
typedef struct RangeIndexTStruct {
  IndexT min;
  IndexT max;
} RangeIndexT;

#define MALLOC(n,typ) (typ *) Calloc((n),typ)
#define FREE(x) Free(x)

#ifdef	NDEBUG
#define	  assert(EX) ((void)0)
#define   debugprint(...)((void)0)
#else
#define	  assert(EX) (void)((EX) || (error("assert(" #EX ") file %s line %d", __FILE__, __LINE__), 0))
#define   debugprint(...) {Rprintf( __VA_ARGS__); R_FlushConsole();}
#endif	/* NDEBUG */

#define ABS(X)(X)
#define LT(A,B) (ABS(A) < ABS(B))
#define LE(A,B) (ABS(A) <= ABS(B))
#define GT(A, B) LT((B), (A))
#define GE(A, B) LE((B), (A))
#define EQ(A,B) (ABS(A) == ABS(B))
#define NE(A,B) (ABS(A) != ABS(B))

#define MOVE(TO,FROM) TO=FROM; 
#define EXCH(A,B,t) {MOVE(t,A) MOVE(A,B) MOVE(B,t)}
#define COMPEXCH(A,B,t) if (LT(B,A)) EXCH(A,B,t)

#define INSERTIONSORT_LIMIT 32


void int_merge_match(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c, IndexT nomatch);
void int_merge_in(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
void int_merge_notin(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
void int_merge_union_all(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_union_exact(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_union_unique(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_intersect_exact(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_intersect_unique(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_symdiff_exact(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_symdiff_unique(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setdiff_exact(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setdiff_unique(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setequal_exact(ValueT *a, IndexT na, ValueT *b, IndexT nb);
int int_merge_setequal_unique(ValueT *a, IndexT na, ValueT *b, IndexT nb);
int int_merge_posdiff_exact(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_posdiff_unique(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_negdiff_exact(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_negdiff_unique(IndexT na, ValueT *b, IndexT nb, ValueT *c);

void int_merge_match_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c, IndexT nomatch);
void int_merge_in_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
void int_merge_notin_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
void int_merge_union_all_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_union_exact_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_union_unique_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_intersect_exact_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_intersect_unique_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_symdiff_exact_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_symdiff_unique_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setdiff_exact_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setdiff_unique_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setequal_exact_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb);
int int_merge_setequal_unique_revab(ValueT *a, IndexT na, ValueT *b, IndexT nb);
int int_merge_posdiff_exact_revab(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_posdiff_unique_revab(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_negdiff_exact_revab(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_negdiff_unique_revab(IndexT na, ValueT *b, IndexT nb, ValueT *c);

void int_merge_match_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c, IndexT nomatch);
void int_merge_in_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
void int_merge_notin_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
void int_merge_union_all_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_union_exact_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_union_unique_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_intersect_exact_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_intersect_unique_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_symdiff_exact_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_symdiff_unique_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setdiff_exact_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setdiff_unique_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setequal_exact_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb);
int int_merge_setequal_unique_reva(ValueT *a, IndexT na, ValueT *b, IndexT nb);
int int_merge_posdiff_exact_reva(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_posdiff_unique_reva(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_negdiff_exact_reva(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_negdiff_unique_reva(IndexT na, ValueT *b, IndexT nb, ValueT *c);

void int_merge_match_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c, IndexT nomatch);
void int_merge_in_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
void int_merge_notin_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
void int_merge_union_all_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_union_exact_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_union_unique_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_intersect_exact_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_intersect_unique_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_symdiff_exact_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_symdiff_unique_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setdiff_exact_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setdiff_unique_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_setequal_exact_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb);
int int_merge_setequal_unique_revb(ValueT *a, IndexT na, ValueT *b, IndexT nb);
int int_merge_posdiff_exact_revb(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_posdiff_unique_revb(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_negdiff_exact_revb(IndexT na, ValueT *b, IndexT nb, ValueT *c);
int int_merge_negdiff_unique_revb(IndexT na, ValueT *b, IndexT nb, ValueT *c);

#endif
