/*
# Fast methods for sorted integers
# (c) 2016 - 2017 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#include "merge.h"

#define REV_A
#define REV_B
#include "merge.c.h"
#include "range.c.h"

#undef REV_A
#include "merge.c.h"
#include "range.c.h"

#define REV_A
#undef REV_B
#include "merge.c.h"
#include "range.c.h"

#undef REV_A
#include "merge.c.h"
#include "range.c.h"

SEXP R_merge_rev(SEXP x_){
  SEXP y_;
  int i, j, n=LENGTH(x_);
  int *intx, *inty;
  double *realx, *realy;
  switch(TYPEOF(x_)) {
  case REALSXP:
    PROTECT( y_ = allocVector(REALSXP,n) );
    realx = REAL(x_);
    realy = REAL(y_);
    for (i=0,j=n-1;i<n;i++,j--)
      realy[i] = -realx[j];
    break;
  case LGLSXP:
    PROTECT( y_ = allocVector(LGLSXP,n) );
    intx = LOGICAL(x_);
    inty = LOGICAL(y_);
    for (i=0,j=n-1;i<n;i++,j--)
      inty[i] = TRUE - intx[j];
    break;
  case INTSXP:
    PROTECT( y_ = allocVector(INTSXP,n) );
    intx = INTEGER(x_);
    inty = INTEGER(y_);
    for (i=0,j=n-1;i<n;i++,j--)
      inty[i] = -intx[j];
    break;
  case CPLXSXP:
  case STRSXP:
  default:
    error("non-implemented type in merge_rev");
  }
  UNPROTECT(1);
  return(y_);
}

SEXP R_merge_match(SEXP x_, SEXP y_, SEXP revx_, SEXP revy_, SEXP nomatch_){
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  int nomatch = asInteger(nomatch_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,nx) );
  int *ret = INTEGER(ret_);
  if (asLogical(revx_)){
    if (asLogical(revy_))
      int_merge_match_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret, nomatch);
    else
      int_merge_match_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret, nomatch);
  }else{
    if (asLogical(revy_))
      int_merge_match_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret, nomatch);
    else
      int_merge_match((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret, nomatch);
  }
  
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_in(SEXP x_, SEXP y_, SEXP revx_, SEXP revy_){
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,nx) );
  int *ret = LOGICAL(ret_);
  if (asLogical(revx_)){
    if (asLogical(revy_))
      int_merge_in_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    else
      int_merge_in_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
  }else{
    if (asLogical(revy_))
      int_merge_in_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    else
      int_merge_in((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
  }
  
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_notin(SEXP x_, SEXP y_, SEXP revx_, SEXP revy_){
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,nx) );
  int *ret = LOGICAL(ret_);
  if (asLogical(revx_)){
    if (asLogical(revy_))
      int_merge_notin_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    else
      int_merge_notin_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
  }else{
    if (asLogical(revy_))
      int_merge_notin_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    else
      int_merge_notin((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
  }
  
  UNPROTECT(1);
  return(ret_);
}



SEXP R_merge_unique(SEXP x_, SEXP revx_){
  int *x = INTEGER(x_);
  int nx = LENGTH(x_);
  int n;
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,nx) );
  int *ret = INTEGER(ret_);
  if (asLogical(revx_)){
    n = int_merge_unique_reva((ValueT *)x,nx,(ValueT *)ret);
  }else{
    n = int_merge_unique((ValueT *)x,nx,(ValueT *)ret);
  }
  if (n < nx){
    SETLENGTH(ret_, n);
  }
  UNPROTECT(1);
  return(ret_);
}


SEXP R_merge_duplicated(SEXP x_, SEXP revx_){
  int *x = INTEGER(x_);
  int nx = LENGTH(x_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,nx) );
  int *ret = INTEGER(ret_);
  if (asLogical(revx_)){
    int_merge_duplicated_reva((ValueT *)x,nx,(ValueT *)ret);
  }else{
    int_merge_duplicated((ValueT *)x,nx,(ValueT *)ret);
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_anyDuplicated(SEXP x_, SEXP revx_){
  int *x = INTEGER(x_);
  int nx = LENGTH(x_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,1) );
  if (asLogical(revx_)){
    LOGICAL(ret_)[0] = int_merge_anyDuplicated_reva((ValueT *)x,nx);
  }else{
    LOGICAL(ret_)[0] = int_merge_anyDuplicated((ValueT *)x,nx);
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_sumDuplicated(SEXP x_, SEXP revx_){
  int *x = INTEGER(x_);
  int nx = LENGTH(x_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  if (asLogical(revx_)){
    INTEGER(ret_)[0] = int_merge_sumDuplicated_reva((ValueT *)x,nx);
  }else{
    INTEGER(ret_)[0] = int_merge_sumDuplicated((ValueT *)x,nx);
  }
  UNPROTECT(1);
  return(ret_);
}



SEXP R_merge_union(SEXP x_, SEXP y_, SEXP revx_, SEXP revy_, SEXP method_){
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,nx+ny) );
  int *ret = INTEGER(ret_);
  if(strcmp(CHAR(STRING_ELT(method_, 0)), "all") == 0) {
    if (asLogical(revx_)){
      if (asLogical(revy_))
        int_merge_union_all_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        int_merge_union_all_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      if (asLogical(revy_))
        int_merge_union_all_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        int_merge_union_all((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else{
    int n;
    if(strcmp(CHAR(STRING_ELT(method_, 0)), "unique") == 0) {
      if (asLogical(revx_)){
        if (asLogical(revy_))
          n = int_merge_union_unique_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
        else
          n = int_merge_union_unique_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      }else{
        if (asLogical(revy_))
          n = int_merge_union_unique_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
        else
          n = int_merge_union_unique((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      }
    }else if(strcmp(CHAR(STRING_ELT(method_, 0)), "exact") == 0) {
      if (asLogical(revx_)){
        if (asLogical(revy_))
          n = int_merge_union_exact_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
        else
          n = int_merge_union_exact_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      }else{
        if (asLogical(revy_))
          n = int_merge_union_exact_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
        else
          n = int_merge_union_exact((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      }
    }else
      error("illegal method");
    if (n < (nx+ny)){
      SETLENGTH(ret_, n);
    }
  }
UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_setdiff(SEXP x_, SEXP y_, SEXP revx_, SEXP revy_, SEXP method_){
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  int n;
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,nx) );
  int *ret = INTEGER(ret_);
  if(strcmp(CHAR(STRING_ELT(method_, 0)), "unique") == 0) {
    if (asLogical(revx_)){
      if (asLogical(revy_))
        n = int_merge_setdiff_unique_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_setdiff_unique_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      if (asLogical(revy_))
        n = int_merge_setdiff_unique_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_setdiff_unique((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else if(strcmp(CHAR(STRING_ELT(method_, 0)), "exact") == 0) {
    if (asLogical(revx_)){
      if (asLogical(revy_))
        n = int_merge_setdiff_exact_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_setdiff_exact_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      if (asLogical(revy_))
        n = int_merge_setdiff_exact_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_setdiff_exact((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else
    error("illegal method");
  if (n < nx){
    SETLENGTH(ret_, n);
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_setequal(SEXP x_, SEXP y_, SEXP revx_, SEXP revy_, SEXP method_){
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,1) );
  if(strcmp(CHAR(STRING_ELT(method_, 0)), "unique") == 0) {
    if (asLogical(revx_)){
      if (asLogical(revy_))
        LOGICAL(ret_)[0] = int_merge_setequal_unique_revab((ValueT *)x,nx,(ValueT *)y,ny);
      else
        LOGICAL(ret_)[0] = int_merge_setequal_unique_reva((ValueT *)x,nx,(ValueT *)y,ny);
    }else{
      if (asLogical(revy_))
        LOGICAL(ret_)[0] = int_merge_setequal_unique_revb((ValueT *)x,nx,(ValueT *)y,ny);
      else
        LOGICAL(ret_)[0] = int_merge_setequal_unique((ValueT *)x,nx,(ValueT *)y,ny);
    }
  }else if(strcmp(CHAR(STRING_ELT(method_, 0)), "exact") == 0) {
    if (asLogical(revx_)){
      if (asLogical(revy_))
        LOGICAL(ret_)[0] = int_merge_setequal_exact_revab((ValueT *)x,nx,(ValueT *)y,ny);
      else
        LOGICAL(ret_)[0] = int_merge_setequal_exact_reva((ValueT *)x,nx,(ValueT *)y,ny);
    }else{
      if (asLogical(revy_))
        LOGICAL(ret_)[0] = int_merge_setequal_exact_revb((ValueT *)x,nx,(ValueT *)y,ny);
      else
        LOGICAL(ret_)[0] = int_merge_setequal_exact((ValueT *)x,nx,(ValueT *)y,ny);
    }
  }else
    error("illegal method");
  UNPROTECT(1);
  return(ret_);
}


SEXP R_merge_intersect(SEXP x_, SEXP y_, SEXP revx_, SEXP revy_, SEXP method_){
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  int n;
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,nx) );
  int *ret = INTEGER(ret_);
  if(strcmp(CHAR(STRING_ELT(method_, 0)), "unique") == 0) {
    if (asLogical(revx_)){
      if (asLogical(revy_))
        n = int_merge_intersect_unique_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_intersect_unique_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      if (asLogical(revy_))
        n = int_merge_intersect_unique_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_intersect_unique((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else if(strcmp(CHAR(STRING_ELT(method_, 0)), "exact") == 0) {
    if (asLogical(revx_)){
      if (asLogical(revy_))
        n = int_merge_intersect_exact_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_intersect_exact_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      if (asLogical(revy_))
        n = int_merge_intersect_exact_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_intersect_exact((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else
    error("illegal method");
  if (n < nx){
    SETLENGTH(ret_, n);
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_symdiff(SEXP x_, SEXP y_, SEXP revx_, SEXP revy_, SEXP method_){
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  int n;
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,nx+ny) );
  int *ret = INTEGER(ret_);
  if(strcmp(CHAR(STRING_ELT(method_, 0)), "unique") == 0) {
    if (asLogical(revx_)){
      if (asLogical(revy_))
        n = int_merge_symdiff_unique_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_symdiff_unique_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      if (asLogical(revy_))
        n = int_merge_symdiff_unique_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_symdiff_unique((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else if(strcmp(CHAR(STRING_ELT(method_, 0)), "exact") == 0) {
    if (asLogical(revx_)){
      if (asLogical(revy_))
        n = int_merge_symdiff_exact_revab((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_symdiff_exact_reva((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      if (asLogical(revy_))
        n = int_merge_symdiff_exact_revb((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
      else
        n = int_merge_symdiff_exact((ValueT *)x,nx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else
    error("illegal method");
  if (n < (nx+ny)){
    SETLENGTH(ret_, n);
  }
  UNPROTECT(1);
  return(ret_);
}


SEXP R_merge_first(SEXP x_, SEXP revx_){
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  if (LENGTH(x_)==0)
    INTEGER(ret_)[0] = NA_INTEGER;
  else if (asLogical(revx_))
    INTEGER(ret_)[0] = -INTEGER(x_)[LENGTH(x_)-1];
  else
    INTEGER(ret_)[0] = INTEGER(x_)[0];
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_last(SEXP x_, SEXP revx_){
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  if (LENGTH(x_)==0)
    INTEGER(ret_)[0] = NA_INTEGER;
  else if (asLogical(revx_))
    INTEGER(ret_)[0] = -INTEGER(x_)[0];
  else
    INTEGER(ret_)[0] = INTEGER(x_)[LENGTH(x_)-1];
  UNPROTECT(1);
  return(ret_);
}



SEXP R_merge_firstin(SEXP rangex_, SEXP y_, SEXP revx_, SEXP revy_){
  int *rx = INTEGER(rangex_);
  int *y = INTEGER(y_);
  int ny = LENGTH(y_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  if (asLogical(revx_)){
    if (asLogical(revy_)){
      INTEGER(ret_)[0] = int_merge_firstin_revab(rx,(ValueT *)y,ny);
    }else{
      INTEGER(ret_)[0] = int_merge_firstin_reva(rx,(ValueT *)y,ny);
    }
  }else{
    if (asLogical(revy_)){
      INTEGER(ret_)[0] = int_merge_firstin_revb(rx,(ValueT *)y,ny);
    }else{
      INTEGER(ret_)[0] = int_merge_firstin(rx,(ValueT *)y,ny); 
    }
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_lastin(SEXP rangex_, SEXP y_, SEXP revx_, SEXP revy_){
  int rx[2];
  rx[0]  = -INTEGER(rangex_)[1];
  rx[1]  = -INTEGER(rangex_)[0];
  int *y = INTEGER(y_);
  int ny = LENGTH(y_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  if (asLogical(revx_)){
    if (!asLogical(revy_)){
      INTEGER(ret_)[0] = -int_merge_firstin_revab(rx,(ValueT *)y,ny);
    }else{
      INTEGER(ret_)[0] = -int_merge_firstin_reva(rx,(ValueT *)y,ny);
    }
  }else{
    if (!asLogical(revy_)){
      INTEGER(ret_)[0] = -int_merge_firstin_revb(rx,(ValueT *)y,ny);
    }else{
      INTEGER(ret_)[0] = -int_merge_firstin(rx,(ValueT *)y,ny); 
    }
  }
  UNPROTECT(1);
  return(ret_);
}



SEXP R_merge_firstnotin(SEXP rangex_, SEXP y_, SEXP revx_, SEXP revy_){
  int *rx = INTEGER(rangex_);
  int *y = INTEGER(y_);
  int ny = LENGTH(y_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  if (asLogical(revx_)){
    if (asLogical(revy_)){
      INTEGER(ret_)[0] = int_merge_firstnotin_revab(rx,(ValueT *)y,ny);
    }else{
      INTEGER(ret_)[0] = int_merge_firstnotin_reva(rx,(ValueT *)y,ny);
    }
  }else{
    if (asLogical(revy_)){
      INTEGER(ret_)[0] = int_merge_firstnotin_revb(rx,(ValueT *)y,ny);
    }else{
      INTEGER(ret_)[0] = int_merge_firstnotin(rx,(ValueT *)y,ny); 
    }
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_lastnotin(SEXP rangex_, SEXP y_, SEXP revx_, SEXP revy_){
  int rx[2];
  rx[0]  = -INTEGER(rangex_)[1];
  rx[1]  = -INTEGER(rangex_)[0];
  int *y = INTEGER(y_);
  int ny = LENGTH(y_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  if (asLogical(revx_)){
    if (!asLogical(revy_)){
      INTEGER(ret_)[0] = -int_merge_firstnotin_revab(rx,(ValueT *)y,ny);
    }else{
      INTEGER(ret_)[0] = -int_merge_firstnotin_reva(rx,(ValueT *)y,ny);
    }
  }else{
    if (!asLogical(revy_)){
      INTEGER(ret_)[0] = -int_merge_firstnotin_revb(rx,(ValueT *)y,ny);
    }else{
      INTEGER(ret_)[0] = -int_merge_firstnotin(rx,(ValueT *)y,ny); 
    }
  }
  UNPROTECT(1);
  return(ret_);
}


SEXP R_merge_rangesect(SEXP rangex_, SEXP y_, SEXP revx_, SEXP revy_){
  int *rx = INTEGER(rangex_);
  int *y = INTEGER(y_);
  int ny = LENGTH(y_);
  int n,nx=abs(rx[1]-rx[0])+1;
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,nx) );
  int *ret = INTEGER(ret_);
  if (asLogical(revx_)){
    if (asLogical(revy_)){
      n = int_merge_rangesect_revab(rx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      n = int_merge_rangesect_reva(rx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else{
    if (asLogical(revy_)){
      n = int_merge_rangesect_revb(rx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      n = int_merge_rangesect(rx,(ValueT *)y,ny,(ValueT *)ret); 
    }
  }
  if (n < nx){
    SETLENGTH(ret_, n);
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_rangediff(SEXP rangex_, SEXP y_, SEXP revx_, SEXP revy_){
  int *rx = INTEGER(rangex_);
  int *y = INTEGER(y_);
  int ny = LENGTH(y_);
  int n,nx=abs(rx[1]-rx[0])+1;
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,nx) );
  int *ret = INTEGER(ret_);
  if (asLogical(revx_)){
    if (asLogical(revy_)){
      n = int_merge_rangediff_revab(rx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      n = int_merge_rangediff_reva(rx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else{
    if (asLogical(revy_)){
      n = int_merge_rangediff_revb(rx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      n = int_merge_rangediff(rx,(ValueT *)y,ny,(ValueT *)ret); 
    }
  }
  if (n < nx){
    SETLENGTH(ret_, n);
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_rangein(SEXP rangex_, SEXP y_, SEXP revx_, SEXP revy_){
  int *rx = INTEGER(rangex_);
  int *y = INTEGER(y_);
  int ny = LENGTH(y_);
  int nx=abs(rx[1]-rx[0])+1;
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,nx) );
  int *ret = LOGICAL(ret_);
  if (asLogical(revx_)){
    if (asLogical(revy_)){
      int_merge_rangein_revab(rx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      int_merge_rangein_reva(rx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else{
    if (asLogical(revy_)){
      int_merge_rangein_revb(rx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      int_merge_rangein(rx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_merge_rangenotin(SEXP rangex_, SEXP y_, SEXP revx_, SEXP revy_){
  int *rx = INTEGER(rangex_);
  int *y = INTEGER(y_);
  int ny = LENGTH(y_);
  int nx=abs(rx[1]-rx[0])+1;
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,nx) );
  int *ret = LOGICAL(ret_);
  if (asLogical(revx_)){
    if (asLogical(revy_)){
      int_merge_rangenotin_revab(rx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      int_merge_rangenotin_reva(rx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }else{
    if (asLogical(revy_)){
      int_merge_rangenotin_revb(rx,(ValueT *)y,ny,(ValueT *)ret);
    }else{
      int_merge_rangenotin(rx,(ValueT *)y,ny,(ValueT *)ret);
    }
  }
  UNPROTECT(1);
  return(ret_);
}
