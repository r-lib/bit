/*
# Fast methods for sorted integers
# (c) 2016 - 2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#include "merge.h"

#ifdef REV_B
#ifdef REV_A
#define FUNCNAME(NAME)NAME##_revab
#else
#define FUNCNAME(NAME)NAME##_revb
#endif
#else
#ifdef REV_A
#define FUNCNAME(NAME)NAME##_reva
#else
#define FUNCNAME(NAME)NAME
#endif
#endif

#ifdef REV_A
#define INIT_A(i,n) i=n-1 
#define A_INC(i) i--
#define INC_A(i)  --i
#define A_MINUS +
#define A_PLUS -
#define DONE_A(i,n) (i < 0)
#define STILL_A(i,n) (i >= 0)
#define A(i)(-a[i])
#define POS_A(i,n)(n-i+1)
#else
#define INIT_A(i,n) i=0 
#define A_INC(i) i++
#define INC_A(i)  ++i
#define A_MINUS -
#define A_PLUS +
#define DONE_A(i,n) (i >= n)
#define STILL_A(i,n) (i < n)
#define A(i)a[i]
#define POS_A(i,n)i
#endif

#ifdef REV_B
#define INIT_B(i,n) i=n-1 
#define B_INC(i) i--
#define INC_B(i)  --i
#define B_MINUS +
#define B_PLUS -
#define DONE_B(i,n) (i < 0)
#define STILL_B(i,n) (i >= 0)
#define B(i)(-b[i])
#define POS_B(i,n)(n-i)
#else
#define INIT_B(i,n) i=0 
#define B_INC(i) i++
#define INC_B(i) ++i
#define B_MINUS -
#define B_PLUS +
#define DONE_B(i,n) (i >= n)
#define STILL_B(i,n) (i < n)
#define B(i)b[i]
#define POS_B(i,n)(i+1)
#endif

#define NEXT_A                                                             \
for(;;){                                                                   \
  if DONE_A(INC_A(ia),na)                                                  \
    goto finb;                                                             \
  if (NE(A(ia), A(ia A_MINUS 1)))                                          \
    break;                                                                 \
}                                                              

#define NEXT_B                                                               \
for(;;){                                                                     \
  if DONE_B(INC_B(ib), nb)                                                   \
    goto fina;                                                               \
  if (NE(B(ib), B(ib B_MINUS 1)))                                            \
    break;                                                                   \
}                                                              

#define NEXT_AB {                                                          \
for(;;){                                                                   \
  if DONE_A(INC_A(ia), na){                                                \
    NEXT_B                                                                 \
    goto finb;                                                             \
  }                                                                        \
  if (NE(A(ia), A(ia A_MINUS 1)))                                          \
    break;                                                                 \
}                                                                          \
for(;;){                                                                   \
  if DONE_B(INC_B(ib), nb)                                                 \
    goto fina;                                                             \
  if (NE(B(ib), B(ib B_MINUS 1)))                                          \
    break;                                                                 \
}                                                                          \
}                                                               


#define FIN_A fina:                                                        \
if STILL_A(ia, na)                                                         \
  c[ic++] = A(A_INC(ia));                                                  \
while STILL_A(ia, na){                                                     \
  if (NE(A(ia), A(ia A_MINUS 1)))                                          \
    c[ic++] = A(ia);                                                       \
  A_INC(ia);                                                               \
}                                                                          \
goto fin;                                                      

#define FIN_B finb:                                                        \
if STILL_B(ib, nb)                                                         \
  c[ic++] = B(B_INC(ib));                                                  \
while STILL_B(ib, nb){                                                     \
  if (NE(B(ib), B(ib B_MINUS 1)))                                          \
    c[ic++] = B(ib);                                                       \
  B_INC(ib);                                                               \
}                                                                          \
goto fin;                                                      




#ifndef REV_B
int FUNCNAME(int_merge_unique)(ValueT *a, IndexT na, ValueT *c){
  IndexT INIT_A(ia, na), ic=0;
  for (;;){
    c[ic++] =  A(ia);
      NEXT_A
  }
    finb:
    return ic;
}

void FUNCNAME(int_merge_duplicated)(ValueT *a, IndexT na, ValueT *c){
  IndexT INIT_A(ia, na); IndexT ic=0;
  ValueT lastvalue;
  while STILL_A(ia, na){
    lastvalue = a[ia];
    c[ic++] = FALSE;
    A_INC(ia);
    while (STILL_A(ia, na) && a[ia] == lastvalue){
      c[ic++] = TRUE;
      A_INC(ia);
    }
  }
}

Rboolean FUNCNAME(int_merge_anyDuplicated)(ValueT *a, IndexT na){
  IndexT INIT_A(ia, na);
  ValueT lastvalue;
  while STILL_A(ia, na){
    lastvalue = a[ia];
    A_INC(ia);
    if (STILL_A(ia, na) && a[ia] == lastvalue){
      return(TRUE);
    }
  }
  return(FALSE);
}

int FUNCNAME(int_merge_sumDuplicated)(ValueT *a, IndexT na){
  IndexT INIT_A(ia, na); IndexT ic=0;
  ValueT lastvalue;
  while STILL_A(ia, na){
    lastvalue = a[ia];
    A_INC(ia);
    while (STILL_A(ia, na) && a[ia] == lastvalue){
      ic++;
      A_INC(ia);
    }
  }
  return(ic);
}
#endif



void FUNCNAME(int_merge_match)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c, IndexT nomatch){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  if (na>0 && nb>0) for (;;){
    if(LT(B(ib), A(ia))){
      B_INC(ib);
      if DONE_B(ib, nb)
        break;
    }else{
      if(LT(A(ia), B(ib))){
        c[ic++] =  nomatch;
      }else{
        c[ic++] =  POS_B(ib,nb);
      }     
      if DONE_A(INC_A(ia), na)
        break;
    }
  }
  while STILL_A(A_INC(ia), na){
    c[ic++] = nomatch;
  }
}

void FUNCNAME(int_merge_in)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  if (na>0 && nb>0) for (;;){
    if(LT(B(ib), A(ia))){
      B_INC(ib);
      if DONE_B(ib, nb)
        break;
    }else{
      if(LT(A(ia), B(ib))){
        c[ic++] =  FALSE;
      }else{
        c[ic++] =  TRUE;
      }     
      if DONE_A(INC_A(ia), na)
        break;
    }
  }
  while STILL_A(A_INC(ia), na){
    c[ic++] = FALSE;
  }
}

void FUNCNAME(int_merge_notin)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  if (na>0 && nb>0) for (;;){
    if(LT(B(ib), A(ia))){
      B_INC(ib);
      if DONE_B(ib, nb)
        break;
    }else{
      if(LT(A(ia), B(ib))){
        c[ic++] =  TRUE;
      }else{
        c[ic++] =  FALSE;
      }     
      if DONE_A(INC_A(ia), na)
        break;
    }
  }
  while STILL_A(A_INC(ia), na){
    c[ic++] = TRUE;
  }
}



// this is the classical stable merge
// which takes from a instead of b in case of ties
void FUNCNAME(int_merge_union_all)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  if (na>0 && nb>0) for (;;){
    if(LT(B(ib), A(ia))){
      c[ic++] =  B(B_INC(ib));
      if DONE_B(ib, nb)
        break;
    }else{
      c[ic++] =  A(A_INC(ia));
      if DONE_A(ia, na)
        break;
    }
  }
  while STILL_A(ia, na){
    c[ic++] = A(A_INC(ia));
  }
  while STILL_B(ib, nb){
    c[ic++] = B(B_INC(ib));
  }
}

int FUNCNAME(int_merge_union_exact)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  if (na>0 && nb>0) for (;;){
    if(LT(B(ib), A(ia))){
      c[ic++] =  B(B_INC(ib));
      if DONE_B(ib, nb)
        break;
    }else{
      c[ic++] =  A(ia);
      if(LT(A(ia), B(ib))){
        A_INC(ia);
      }else{
        A_INC(ia);
        B_INC(ib);
        if DONE_B(ib, nb)
          break;
      }
      if DONE_A(ia, na)
        break;
    } 
  }
  while STILL_A(ia, na){
    c[ic++] = A(A_INC(ia));
  }
  while STILL_B(ib, nb){
    c[ic++] = B(B_INC(ib));
  }
  return ic;
}

int FUNCNAME(int_merge_union_unique)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  for (;;){
    if(LT(B(ib), A(ia))){
      c[ic++] =  B(ib);
      NEXT_B
    }else{
      c[ic++] =  A(ia);
      if(LT(A(ia), B(ib)))
        NEXT_A
        else
          NEXT_AB
    } 
  }
  FIN_A
    FIN_B
    fin:
    return ic;
}



int FUNCNAME(int_merge_setdiff_exact)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  if (na>0 && nb>0) for (;;){
    if(LT(A(ia), B(ib))){
      c[ic++] =  A(A_INC(ia));
      if DONE_A(ia, na)
        break;
    }else{
      if(LT(B(ib), A(ia))){
        B_INC(ib);
      }else{
        B_INC(ib);
        A_INC(ia);
        if DONE_A(ia, na)
          break;
      }
      if DONE_B(ib, nb)
        break;
    }
  }
  while STILL_A(ia, na){
    c[ic++] =  A(A_INC(ia));
  }
  return ic;
}

int FUNCNAME(int_merge_setdiff_unique)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  if (na>0 && nb>0) for (;;){
    if(LT(A(ia), B(ib))){
      c[ic++] =  A(ia);
      NEXT_A
    }else{
      if (LT(B(ib),A(ia)))
        NEXT_B
        else
          NEXT_AB
    }
  }
  FIN_A
    finb:
    fin:
    return ic;
}


int FUNCNAME(int_merge_setequal_exact)(ValueT *a, IndexT na, ValueT *b, IndexT nb){
  if (na!=nb)
      return FALSE;
  IndexT INIT_A(ia, na), INIT_B(ib, nb);
  if (na>0) for (;;){
    if(EQ(A(ia), B(ib))){
      A_INC(ia);
      if DONE_A(ia, na)
        break;
      B_INC(ib);
    }else{
      return FALSE;
    }
  }
  return TRUE;
}


int FUNCNAME(int_merge_setequal_unique)(ValueT *a, IndexT na, ValueT *b, IndexT nb){
  IndexT INIT_A(ia, na), INIT_B(ib, nb);
  if (na>0 && nb>0) for (;;){
    if(EQ(A(ia), B(ib))){
      NEXT_AB
    }else{
      return FALSE;
    }
  }
  fina:
  finb:
  if (DONE_A(ia, na)==DONE_A(ib, nb))
    return TRUE;
  else
    return FALSE;
}


int FUNCNAME(int_merge_intersect_exact)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  if (na>0 && nb>0) for (;;){
    if(LT(B(ib), A(ia))){
      B_INC(ib);
      if DONE_B(ib, nb)
        break;
    }else{
      if (LT(A(ia),B(ib))){
        A_INC(ia);
      }else{
        c[ic++] =  A(A_INC(ia));
        B_INC(ib);
        if DONE_B(ib, nb)
          break;
      }
      if DONE_A(ia, na)
        break;
    }
  }
  return ic;
}


int FUNCNAME(int_merge_intersect_unique)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  if (na>0 && nb>0) for (;;){
    if(LT(B(ib), A(ia))){
      NEXT_B
    }else{
      if (LT(A(ia),B(ib))){
        NEXT_A
      }else{
        c[ic++] =  A(ia);
        NEXT_AB
      }
    }
  }
  fina:
    finb:
    return ic;
}

int FUNCNAME(int_merge_symdiff_exact)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  if (na>0 && nb>0) for (;;){
    if(LT(B(ib), A(ia))){
      c[ic++] =  B(B_INC(ib));
      if DONE_B(ib, nb)
        goto fina;
    }else if (LT(A(ia),B(ib))){
      c[ic++] =  A(A_INC(ia));
      if DONE_A(ia, na)
        goto finb;
    }else{
      A_INC(ia);
      B_INC(ib);
      if DONE_A(ia, na)
        goto finb;
      if DONE_B(ib, nb)
        goto fina;
    }
  }
  fina:
    while STILL_A(ia, na){
      c[ic++] =  A(A_INC(ia));
    }
    goto fin;
  finb:
    while STILL_B(ib, nb){
      c[ic++] =  B(B_INC(ib));
    }
    fin:
      return ic;
}


int FUNCNAME(int_merge_symdiff_unique)(ValueT *a, IndexT na, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, na), INIT_B(ib, nb), ic=0;
  for (;;){
    if(LT(B(ib), A(ia))){
      c[ic++] =  B(ib);
      NEXT_B
    }else if (LT(A(ia),B(ib))){
      c[ic++] =  A(ia);
      NEXT_A
    }else{
      NEXT_AB
    }
  }
  FIN_A
    FIN_B
    fin:
    return ic;
}

#undef INIT_A
#undef A_INC
#undef INC_A
#undef A_MINUS
#undef A_PLUS
#undef DONE_A
#undef STILL_A
#undef POS_A
#undef A

#undef INIT_B
#undef B_INC
#undef INC_B
#undef B_MINUS
#undef B_PLUS
#undef DONE_B
#undef STILL_B
#undef POS_B
#undef B

#undef NEXT_A
#undef NEXT_B
#undef NEXT_AB
#undef FIN_A
#undef FIN_B

#undef FUNCNAME
