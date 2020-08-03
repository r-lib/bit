/*
#  integer utilities 
# (c) 2016 - 2017 Jens Oehlschlägel
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#include <R.h>
#include <Rinternals.h>

// remember that NA_INTEGER==-2147483648
// and the allowed range of R's integer is -2147483647..-2147483647

// SEXP R_get_refcnt(SEXP x_){
//   SEXP y_;
//   PROTECT( y_ = allocVector(INTSXP,1) );
//   INTEGER(y_)[0] = NAMED(x_);
//   UNPROTECT(1);
//   return(y_);
// }

// SEXP R_set_refcnt(SEXP x_, SEXP refcnt_){
//   int refcnt = asInteger(refcnt_);
//   PROTECT(x_);
//   SET_NAMED(x_, refcnt);
//   UNPROTECT(1);
//   return(R_NilValue);
// }

// SEXP R_bit_named(SEXP x){
// SEXP ret_; 
// PROTECT( ret_ = allocVector(INTSXP,1) );
// INTEGER(ret_)[0] = NAMED(x);
// UNPROTECT(1);
// return ret_;
// }


SEXP R_bitwhich_representation(SEXP x_){
  SEXP ret_;
  if (TYPEOF(x_) == LGLSXP){
    if (LENGTH(x_)){
      PROTECT( ret_ = allocVector(LGLSXP,1));
      LOGICAL(ret_)[0] = LOGICAL(x_)[0];
    }else{
      PROTECT( ret_ = allocVector(LGLSXP,0));
    }
  }else{
    PROTECT( ret_ = allocVector(INTSXP,1));
    if (INTEGER(x_)[0]<0)  
      INTEGER(ret_)[0] = -1;
    else
      INTEGER(ret_)[0] = 1;
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_get_length(SEXP x_){
  SEXP y_;
  PROTECT( y_ = allocVector(INTSXP,1));
  INTEGER(y_)[0] = LENGTH(x_);
  UNPROTECT(1);
  return(y_);
}

SEXP R_still_identical(
    SEXP x_
  , SEXP y_
)
{
  SEXP ret_;
  Rboolean ret;
  if(!isVectorAtomic(x_)){
    error("SEXP is not atomic vector");
		return R_NilValue;
  }
  if (TYPEOF(x_)!=TYPEOF(y_)){
    error("vectors don't have identic type");
		return R_NilValue;
  }
  //somehow is DATAPTR not declared: ret = DATAPTR(x_)==DATAPTR(y_) ? TRUE : FALSE;
  switch (TYPEOF(x_)) {
  case CHARSXP:
    ret = CHAR(x_)==CHAR(y_) ? TRUE : FALSE;
    break;
  case LGLSXP:
    ret = LOGICAL(x_)==LOGICAL(y_) ? TRUE : FALSE;
  case INTSXP:
    ret = INTEGER(x_)==INTEGER(y_) ? TRUE : FALSE;
    break;
  case REALSXP:
    ret = REAL(x_)==REAL(y_) ? TRUE : FALSE;
    break;
  case CPLXSXP:
    ret = COMPLEX(x_)==COMPLEX(y_) ? TRUE : FALSE;
    break;
  case STRSXP:
    ret = STRING_PTR(x_)==STRING_PTR(y_) ? TRUE : FALSE;
    break;
  case VECSXP:
    ret = VECTOR_PTR(x_)==VECTOR_PTR(y_) ? TRUE : FALSE;
  case RAWSXP:
    ret = RAW(x_)==RAW(y_) ? TRUE : FALSE;
    break;
  default:
    error("unimplemented type in truly.identical");
  return R_NilValue;
  }
  if (LENGTH(x_)!=LENGTH(y_)){
    ret = FALSE;
  }
  PROTECT( ret_ = allocVector(LGLSXP, 1) );
  INTEGER(ret_)[0] = ret;
  UNPROTECT(1);
  return ret_;
}


SEXP R_copy_vector(SEXP x_, SEXP revx_){
  SEXP y_;
  int revx = asLogical(revx_);
  int i, j, n=LENGTH(x_);
  int *intx, *inty;
  double *realx, *realy;
  if(!isVectorAtomic(x_)){
    error("SEXP is not atomic vector");
  }
  switch(TYPEOF(x_)) {
  case REALSXP:
    PROTECT( y_ = allocVector(REALSXP,n) );
    realx = REAL(x_);
    realy = REAL(y_);
    if (revx)
      for (i=0,j=n-1;i<n;i++,j--)
        realy[i] = -realx[j];
    else
    for (i=0;i<n;i++)
      realy[i] = realx[i];
    break;
  case LGLSXP:
    PROTECT( y_ = allocVector(LGLSXP,n) );
    intx = LOGICAL(x_);
    inty = LOGICAL(y_);
    if (revx)
      for (i=0,j=n-1;i<n;i++,j--)
        inty[i] = -intx[j];
    else
        for (i=0;i<n;i++)
      inty[i] = intx[i];
    break;
  case INTSXP:
    PROTECT( y_ = allocVector(INTSXP,n) );
    intx = INTEGER(x_);
    inty = INTEGER(y_);
    if (revx)
      for (i=0,j=n-1;i<n;i++,j--)
        inty[i] = -intx[j];
    else
        for (i=0;i<n;i++)
      inty[i] = intx[i];
    break;
  case CPLXSXP:
  case STRSXP:
  default:
    error("non-implemented type in copy_vector");
  }
  UNPROTECT(1);
  return(y_);
}


SEXP R_reverse_vector(SEXP x_){
  SEXP y_;
  int i, j, n=LENGTH(x_);
  int *intx, *inty;
  double *realx, *realy;
  if(!isVectorAtomic(x_)){
    error("SEXP is not atomic vector");
  }  
  switch(TYPEOF(x_)) {
  case REALSXP:
    PROTECT( y_ = allocVector(REALSXP,n) );
    realx = REAL(x_);
    realy = REAL(y_);
      for (i=0,j=n-1;i<n;i++,j--)
        realy[i] = realx[j];
    break;
  case LGLSXP:
    PROTECT( y_ = allocVector(LGLSXP,n) );
    intx = LOGICAL(x_);
    inty = LOGICAL(y_);
      for (i=0,j=n-1;i<n;i++,j--)
        inty[i] = intx[j];
    break;
  case INTSXP:
    PROTECT( y_ = allocVector(INTSXP,n) );
    intx = INTEGER(x_);
    inty = INTEGER(y_);
      for (i=0,j=n-1;i<n;i++,j--)
        inty[i] = intx[j];
    break;
  case CPLXSXP:
  case STRSXP:
  default:
    error("non-implemented type in reverse_vector");
  }
  UNPROTECT(1);
  return(y_);
}




SEXP R_firstNA(SEXP x_){
  SEXP y_;
  int i, n=LENGTH(x_);
  int *intx;
  double *realx;
  PROTECT( y_ = allocVector(INTSXP,1) );
  int *y = INTEGER(y_);
  y[0] = 0;
  switch(TYPEOF(x_)) {
  case REALSXP:
    realx = REAL(x_);
    for (i=0;i<n;i++){
      if (ISNA(realx[i])){
        y[0] = i+1;
        break;
      }
    }
    break;
  case LGLSXP:
    intx = LOGICAL(x_);
    for (i=0;i<n;i++){
      if (intx[i] == NA_LOGICAL){
        y[0] = i+1;
        break;
      }
    }
    break;
  case INTSXP:
    intx = INTEGER(x_);
    for (i=0;i<n;i++){
      if (intx[i] == NA_INTEGER){
        y[0] = i+1;
        break;
      }
    }
    break;
  case CPLXSXP:
  case STRSXP:
  default:
    error("non-implemented type in firstNA");
  }
  UNPROTECT(1);
  return(y_);
}


// determine min, max and number of NAs
SEXP R_range_na(SEXP x_){
  int *x = INTEGER(x_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,3) );
  int *ret = INTEGER(ret_);
  int i, n=LENGTH(x_);
  int min=NA_INTEGER;
  int max=NA_INTEGER;
  int countna=0;
  register int t;
  for (i=0; i<n; i++){
    if (x[i] == NA_INTEGER)
      countna++;
    else{
      min = x[i];
      max = x[i];
      break;
    }
  }
  for ( ; i<n; i++){
    t = x[i];
    if (t<min){
      if (t == NA_INTEGER)
        countna++;
      else
        min = t;
    }else  if (t>max)
      max = t;
  }
  ret[0] = min;
  ret[1] = max;
  ret[2] = countna;
  UNPROTECT(1);
  return(ret_);
}

// dito but copy all of x_ but zero to y_
// and return y_ with range_na as attribute
SEXP R_range_nanozero(SEXP x_){
  int ix, iy, n=LENGTH(x_);
  int min=NA_INTEGER;
  int max=NA_INTEGER;
  int countna=0;
  SEXP y_,ret_;
  PROTECT( ret_ = allocVector(INTSXP,3) );
  PROTECT( y_ = allocVector(INTSXP,n) );
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int *ret = INTEGER(ret_);
  register int t;
  for (ix=0,iy=0; ix<n; ix++){
    if (x[ix] == NA_INTEGER){
      y[iy++] = x[ix];
      countna++;
    }else if(x[ix]!=0){
      min = x[ix];
      max = x[ix];
      y[iy++] = x[ix++];
      break;
    }
  }
  for ( ; ix<n; ix++){
    t = x[ix];
    if (t!=0){
      y[iy++] = t;
      if (t<min){
        if (t == NA_INTEGER)
          countna++;
        else
          min = t;
      }else if (t>max){
        max = t;
      }
    }
  }
  if (iy<ix){
    SETLENGTH(y_, iy);
  }
  ret[0] = min;
  ret[1] = max;
  ret[2] = countna;

  setAttrib(y_, install("range_na"), ret_);
  UNPROTECT(2);
  return(y_);
}

// determine min, max, sort NAs to one end (and count them), determine (un)sortedness
// and return y_ with range_na as attribute (min,max,sumNA,isUnsorted)
SEXP R_range_sortna(SEXP x_, SEXP decreasing_, SEXP na_last_){
  int ix, iy, n=LENGTH(x_);
  int na_last = asLogical(na_last_);
  int decreasing = asLogical(decreasing_);
  int min=NA_INTEGER;
  int max=NA_INTEGER;
  int countna=0;
  int unsorted=FALSE;
  SEXP y_,ret_;
  PROTECT( ret_ = allocVector(INTSXP,4) );
  PROTECT( y_ = allocVector(INTSXP,n) );
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int *ret = INTEGER(ret_);
  register int t, last=0; // zero assignment just to quiet compiler
  
  if (na_last == FALSE){
    // all NAs to start
    for (ix=n-1,iy=n; ix>=0; ix--){
      if (x[ix] != NA_INTEGER){
        min = x[ix];
        max = x[ix];
        last = x[ix];
        y[--iy] = x[ix--];
        break;
      }
    }
    if (decreasing)
      for ( ; ix>=0; ix--){
        t = x[ix];
        if (t != NA_INTEGER){
          y[--iy] = t;
          if (t<min){
            min = t;
          }else if (t>max){
            max = t;
          }
          if (t < last){
            unsorted = TRUE;
            ix--;
            break;
          }
          last = t;
        }
      }
      else
        for ( ; ix>=0; ix--){
          t = x[ix];
          if (t != NA_INTEGER){
            y[--iy] = t;
            if (t<min){
              min = t;
            }else if (t>max){
              max = t;
            }
            if (t > last){
              unsorted = TRUE;
              ix--;
              break;
            }
            last = t;
          }
        }
      for ( ; ix>=0; ix--){
      t = x[ix];
      if (t != NA_INTEGER){
        y[--iy] = t;
        if (t<min){
          min = t;
        }else if (t>max){
          max = t;
        }
      }
    }
    countna = iy;
    while(iy>0){
      y[--iy] = NA_INTEGER;
    }  
  }else{
    // all nonNA to start
    for (ix=0,iy=0; ix<n; ix++){
      if (x[ix] != NA_INTEGER){
        min = x[ix];
        max = x[ix];
        last = x[ix];
        y[iy++] = x[ix++];
        break;
      }
    }
    if (decreasing)
      for ( ; ix<n; ix++){
        t = x[ix];
        if (t != NA_INTEGER){
          y[iy++] = t;
          if (t<min){
            min = t;
          }else if (t>max){
            max = t;
          }
          if (t > last){
            unsorted = TRUE;
            ix++;
            break;
          }
          last = t;
        }
      }
    else
      for ( ; ix<n; ix++){
        t = x[ix];
        if (t != NA_INTEGER){
          y[iy++] = t;
          if (t<min){
            min = t;
          }else if (t>max){
            max = t;
          }
          if (t < last){
            unsorted = TRUE;
            ix++;
            break;
          }
          last = t;
        }
      }
    for ( ; ix<n; ix++){
      t = x[ix];
      if (t != NA_INTEGER){
        y[iy++] = t;
        if (t<min){
          min = t;
        }else if (t>max){
          max = t;
        }
      }
    }
    if (na_last ==  NA_INTEGER){
      // drop all NAs
      countna = 0;
      SETLENGTH(y_, iy);
    }else{
      // all NAs to end
      countna = n - iy;
      while(iy<n){
        y[iy++] = NA_INTEGER;
      }  
    }
  }
  ret[0] = min;
  ret[1] = max;
  ret[2] = countna;
  ret[3] = unsorted;
  
  setAttrib(y_, install("range_sortna"), ret_);
  UNPROTECT(2);
  return(y_);
}

