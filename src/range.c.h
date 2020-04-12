/*
# Fast methods for sorted integers (where the first argument is a range)
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
#define INIT_A(i,r) i=r[1]
#define A_INC(i) i--
#define INC_A(i)  --i
#define A_MINUS +
#define A_PLUS -
#define DONE_A(i,r) (i < r[0])
#define STILL_A(i,r) (i >= r[0])
#define A(i)(-i)
#else
#define INIT_A(i,r) i=r[0] 
#define A_INC(i) i++
#define INC_A(i)  ++i
#define A_MINUS -
#define A_PLUS +
#define DONE_A(i,r) (i > r[1])
#define STILL_A(i,r) (i <= r[1])
#define A(i)i
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
#else
#define INIT_B(i,n) i=0 
#define B_INC(i) i++
#define INC_B(i) ++i
#define B_MINUS -
#define B_PLUS +
#define DONE_B(i,n) (i >= nb)
#define STILL_B(i,n) (i < nb)
#define B(i)b[i]
#endif


ValueT FUNCNAME(int_merge_firstin)(IndexT *ra, ValueT *b, IndexT nb){
  IndexT INIT_A(ia, ra), INIT_B(ib, nb), c=NA_INTEGER;
  if (ra[0]<=ra[1] && nb>0) for (;;){
    if(LT(A(ia), B(ib))){
      A_INC(ia);
      if DONE_A(ia, ra)
        break;
    }else{
      if(LT(B(ib), A(ia))){
        B_INC(ib);
      }else{
        c =  A(ia);
        break;
      }
      if DONE_B(ib, nb)
        break;
    }
  }
  return c;
}

ValueT FUNCNAME(int_merge_firstnotin)(IndexT *ra, ValueT *b, IndexT nb){
  IndexT INIT_A(ia, ra), INIT_B(ib, nb), c=NA_INTEGER;
  if (ra[0]<=ra[1] && nb>0) for (;;){
    if(LT(A(ia), B(ib))){
      c =  A(ia);
      goto done;
    }else{
      if(LT(B(ib), A(ia))){
        B_INC(ib);
      }else{
        B_INC(ib);
        A_INC(ia);
        if DONE_A(ia, ra)
          goto done;
      }
      if DONE_B(ib, nb){
        break;
      }
    }
  }
  if STILL_A(ia, ra){
    c =  A(ia);
  }
  done:  
  return c;
}

int FUNCNAME(int_merge_rangesect)(IndexT *ra, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, ra), INIT_B(ib, nb), ic=0;
  if (ra[0]<=ra[1] && nb>0) for (;;){
    if(LT(A(ia), B(ib))){
      A_INC(ia);
      if DONE_A(ia, ra)
        break;
    }else{
      if(LT(B(ib), A(ia))){
        B_INC(ib);
      }else{
        c[ic++] =  A(ia);
        B_INC(ib);
        A_INC(ia);
        if DONE_A(ia, ra)
          break;
      }
      if DONE_B(ib, nb)
        break;
    }
  }
  return ic;
}


int FUNCNAME(int_merge_rangediff)(IndexT *ra, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, ra), INIT_B(ib, nb), ic=0;
  if (ra[0]<=ra[1] && nb>0) for (;;){
    if(LT(A(ia), B(ib))){
      c[ic++] =  A(A_INC(ia));
      if DONE_A(ia, ra)
        break;
    }else{
      if(LT(B(ib), A(ia))){
        B_INC(ib);
      }else{
        B_INC(ib);
        A_INC(ia);
        if DONE_A(ia, ra)
          break;
      }
      if DONE_B(ib, nb)
        break;
    }
  }
  while STILL_A(ia, ra){
    c[ic++] =  A(A_INC(ia));
  }
  return ic;
}


void FUNCNAME(int_merge_rangein)(ValueT *ra, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, ra), INIT_B(ib, nb), ic=0;
  if (ra[0]<=ra[1] && nb>0) for (;;){
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
      A_INC(ia);
      if DONE_A(ia, ra)
        break;
    }
  }
  while STILL_A(ia, ra){
    c[ic++] = FALSE;
    A_INC(ia);
  }
}


void FUNCNAME(int_merge_rangenotin)(ValueT *ra, ValueT *b, IndexT nb, ValueT *c){
  IndexT INIT_A(ia, ra), INIT_B(ib, nb), ic=0;
  if (ra[0]<=ra[1] && nb>0) for (;;){
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
      A_INC(ia);
      if DONE_A(ia, ra)
        break;
    }
  }
  while STILL_A(ia, ra){
    c[ic++] = TRUE;
    A_INC(ia);
  }
}

#undef INIT_A
#undef A_INC
#undef INC_A
#undef A_MINUS
#undef A_PLUS
#undef DONE_A
#undef STILL_A
#undef A

#undef INIT_B
#undef B_INC
#undef INC_B
#undef B_MINUS
#undef B_PLUS
#undef DONE_B
#undef STILL_B
#undef B

#undef FUNCNAME

