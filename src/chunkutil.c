/*
# C-Code for chunk utilities
# (c) 2012 Jens Oehlschaegel
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#include <R.h>
#define USE_RINTERNALS
#include <Rinternals.h>

SEXP R_bit_vecseq(SEXP x_, SEXP y_)
{
    int *x,*y,*ret;
    register int val, lim;
    R_len_t K,k,n,i;
    SEXP ret_;
    // if (!isInteger(x_))
      // error("x must be an integer vector");
    // if (!isInteger(y_))
      // error("y must be an integer vector");
    K = LENGTH(x_);
    // if (LENGTH(y_) != K) error("x and y must be the same length");
    
    x = INTEGER(x_);
    y = INTEGER(y_);
    
    n = 0;
    for (k=0; k<K; k++) 
      n += x[k] < y[k] ? y[k] - x[k] + 1 : x[k] - y[k] + 1;
    ret_ = PROTECT(allocVector(INTSXP, n));
    ret = INTEGER(ret_);
    
    i = 0;
    for (k=0; k<K; k++) {
      lim = y[k];
      val = x[k];
      if (val < lim){
        while(val<=lim){
          ret[i++] = val++;
        }
      }else{
        while(val>=lim){
          ret[i++] = val--;
        }
      }
    }
    UNPROTECT(1);
    return(ret_);
}
