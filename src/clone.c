/*
#  cloning and identity querying
# (c) 2014 Jens Oehlschlägel
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#include <R.h>
#include <Rinternals.h>


SEXP R_duplicate(SEXP x_){
  SEXP y_ = PROTECT(duplicate(x_));
  UNPROTECT(1);
  return(y_);
}
