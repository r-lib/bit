/*
# C-Code for 1-bit boolean vectors for R
# first bit is stored in lowest (rightmost) bit of forst word
# remember that rightshifting is dangerous because we use the sign position
# (c) 2012 Jens Oehlschägel
# Licence: GPL2
# Provided 'as is', use at your own risk
*/

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>

#include "merge.h"
#include "sort.h"


// Configuration: set this to 32 or 64 and keep in sync with .BITS in bit.R
#define BITS 32
// Configuration: set this to BITS-1
#define LASTBIT 31
#define FALSE 0
#define TRUE 1

/*
& bitwise and
| bitwise or
^ bitwise xor
~ bitwise not
*/

#if BITS == 64
typedef unsigned long long int bitint;
#else
typedef unsigned int bitint;
#endif

static bitint *mask0, *mask1;

static void bit_init(int   bits){
  if (bits != BITS)
    error("R .BITS and C BITS are not in sync");
  if (bits-1 != LASTBIT)
    error("R .BITS and C LASTBIT are not in sync");
  mask0 = calloc(BITS, sizeof(bitint));
  mask1 = calloc(BITS, sizeof(bitint));
  bitint b = 1;
  int i;
  for (i=0; i<BITS; i++){
    mask1[i] = (bitint) b;
    mask0[i] = (bitint) ~b;
    //Rprintf("i=%d mask0[i]=%d mask1[i]=%d\n", i, mask0[i], mask1[i]);
    b = b << 1;
  }
}

static void bit_done(void){
  free(mask0);
  free(mask1);
}


SEXP R_bit_init(SEXP bits_){
  int bits = asInteger(bits_);
  bit_init(bits);
  return R_NilValue;
}
SEXP R_bit_done(void){
  bit_done();
  return R_NilValue;
}


/*
copy 'n' bits from 'bfrom' to 'bto' with offset 'oto'
NOTE that remaining target bits AFTER the copied area are overwritten with zero
*/
static void bit_shiftcopy(
    bitint *bsource /* bit source */
, bitint *btarget /* bit target */
, int otarget     /* offset target */
, int n      /* number of bits to copy */
){
  register int upshift = otarget % BITS;    /* this is used for leftshifting bsource to meet btarget */
int downshift = BITS - upshift;
register int downshiftrest = downshift - 1;    /* this is used for downshiftresting bsource to meet btarget */

int source_i  = 0;
int target_i  = otarget;
int source_i1 = n - 1;
int target_i1 = otarget + n - 1;

int source_j  = source_i  / BITS;
int target_j  = target_i  / BITS;
int source_j1 = source_i1 / BITS;
int target_j1 = target_i1 / BITS;

//int source_k  = source_i  % BITS;
//int target_k  = target_i  % BITS;
//int source_k1 = source_i1 % BITS;
//int target_k1 = target_i1 % BITS;

if (upshift){
  /* clean the positions of the new bits for the following OR */
  btarget[target_j] = (((btarget[target_j] << downshift) >> 1) & mask0[LASTBIT]) >> downshiftrest; /* special treatment of the leftmost bit in downshift to make sure the downshift is filled with zeros */
  /* now copy into part using OR */
  btarget[target_j] |= bsource[source_j] << upshift;
  target_j++;
  for (; source_j<source_j1; source_j++,target_j++){
    /* & mask0[LASTBIT] */
    btarget[target_j] = ( ((bsource[source_j] >> 1) & mask0[LASTBIT]) >> downshiftrest ) | ( bsource[source_j+1] << upshift ); /* special treatment of the leftmost bit in downshift to make sure the downshift is filled with zeros */
  }
}else{
  for (; source_j<source_j1; source_j++,target_j++){
    btarget[target_j] = bsource[source_j];
  }
}
if (target_j==target_j1){
  if (upshift){
    /* clean the positions of the new bits for the following OR */
    btarget[target_j1] = ( ((btarget[target_j1] >> 1) & mask0[LASTBIT]) >> (upshift-1)) << upshift;
    /*  & mask0[LASTBIT] */
    btarget[target_j1] |= ( ((bsource[source_j1] >> 1) & mask0[LASTBIT]) >> downshiftrest ); /* special treatment of the leftmost bit in downshift to make sure the downshift is filled with zeros */
  }else{
    btarget[target_j1] = bsource[source_j1];
  }
}
}

// { --- bit reversal ---

// copy bits to reverse order
static void bit_reverse(bitint *s, bitint *t, int n){
  register int sk;
  register int sj;
  register bitint sw;
  register int nj,tj;
  register int nk,tk;
  nj=tj=(n-1)/BITS;
  nk=tk=(n-1)%BITS;
  register bitint tw = t[tj];
  for (sj=0; sj<nj; sj++){
    sw = s[sj];
    for(sk=0; sk<BITS ;sk++,tk--){
      //Rprintf("main: nj=%d nk=%d ni=%d   sj=%d sk=%d si=%d  tj=%d tk=%d ti=%d\n", nj, nk, (nj*BITS+nk), sj, sk, (sj*BITS+sk), tj, tk, (tj*BITS+tk));
      // target maintenance at the beginning of the loop seems redundant, 
      // but guarantees that target word t[--tj] exists
      if (tk<0){  
        t[tj] = tw;
        tw = t[--tj];
        tk = LASTBIT;
      }
      if (sw & mask1[sk])
        tw |= mask1[tk];
      else
        tw &= mask0[tk];
    }
  }
  if (sj==nj){
    sw = s[sj];
    for(sk=0; sk <= nk ;sk++,tk--){
      //Rprintf("post: nj=%d nk=%d ni=%d   sj=%d sk=%d si=%d  tj=%d tk=%d ti=%d\n", nj, nk, (nj*BITS+nk), sj, sk, (sj*BITS+sk), tj, tk, (tj*BITS+tk));
      if (tk<0){
        t[tj] = tw;
        tw = t[--tj];
        tk = LASTBIT;
      }
      if (sw & mask1[sk])
        tw |= mask1[tk];
      else
        tw &= mask0[tk];
    }
  }
  t[tj] = tw;
}


// } --- bit reversal ---



// { --- transfer between bit and logical for certain range of b ---

// get logical from range of bit
static void bit_get(bitint *b, int *l, int from, int to){
  from--;
  to--;
  register bitint word;
  register int i=0;
  register int k=from%BITS;
  register int j=from/BITS;
  register int k1=to%BITS;
  register int j1=to/BITS;
  if (j<j1){
    word = b[j];
    for(; k<BITS; k++){
      l[i++] = word & mask1[k] ? 1 : 0;
    }
    for (j++; j<j1; j++){
      word = b[j];
      for(k=0 ;k<BITS ;k++){
        l[i++] = word & mask1[k] ? 1 : 0;
      }
    }
    k=0;
  }
  if (j==j1 && k<=k1){
    word = b[j];
    for(; k<=k1 ;k++){
      l[i++] = word & mask1[k] ? 1 : 0;
    }
  }
}

// set range of bit from logical
static void bit_set(bitint *b, int *l, int from, int to){
  from--;
  to--;
  register bitint word = 0;  /* this init only to keep compiler quiet */
    register int i=0;
    register int k=from%BITS;
    register int j=from/BITS;
    register int k1=to%BITS;
    register int j1=to/BITS;
    if (j<j1){
      word = b[j];
      for(; k<BITS; k++,i++){
        if (l[i]==FALSE || l[i]==NA_INTEGER)
          word &= mask0[k];
        else
          word |= mask1[k];
      }
      b[j] = word;
      for (j++; j<j1; j++){
        word = b[j];
        for(k=0 ;k<BITS; k++,i++){
          if (l[i]==FALSE || l[i]==NA_INTEGER)
            word &= mask0[k];
          else
            word |= mask1[k];
        }
        b[j] = word;
      }
      k = 0;
    }
    if (j==j1 && k<=k1){
      word = b[j];
      for(; k<=k1 ;k++,i++){
        if (l[i]==FALSE || l[i]==NA_INTEGER)
          word &= mask0[k];
        else
          word |= mask1[k];
      }
      b[j] = word;
    }
}




// dito but recycles logical
static void bit_set_recycle(bitint *b, int *l, int from, int to, int nl){
  from--;
  to--;
  register bitint word = 0;  /* this init only to keep compiler quiet */
    register int il=0;
    register int k=from%BITS;
    register int j=from/BITS;
    register int k1=to%BITS;
    register int j1=to/BITS;
    if (j<j1){
      word = b[j];
      for(; k<BITS; k++){
        if (l[il]==FALSE || l[il]==NA_INTEGER)
          word &= mask0[k];
        else
          word |= mask1[k];
        if (++il>=nl)
          il -= nl; // recycle l
      }
      b[j] = word;
      for (j++; j<j1; j++){
        word = b[j];
        for(k=0 ;k<BITS; k++){
          if (l[il]==FALSE || l[il]==NA_INTEGER)
            word &= mask0[k];
          else
            word |= mask1[k];
          if (++il>=nl)
            il -= nl; // recycle l
        }
        b[j] = word;
      }
      k = 0;
    }
    if (j==j1 && k<=k1){
      word = b[j];
      for(; k<=k1 ;k++){
        if (l[il]==FALSE || l[il]==NA_INTEGER)
          word &= mask0[k];
        else
          word |= mask1[k];
        if (++il>=nl)
          il -= nl; // recycle l
      }
      b[j] = word;
    }
}










// dito but with scalar logical
static void bit_set_one(bitint *b, int l, int from, int to){
  from--;
  to--;
  register bitint word = 0;  /* this init only to keep compiler quiet */
    register int k=from%BITS;
    register int j=from/BITS;
    register int k1=to%BITS;
    register int j1=to/BITS;
    if (l==NA_INTEGER)
      l = FALSE;
    if (j<j1){
      word = b[j];
      for(; k<BITS; k++){
        if (l==FALSE)
          word &= mask0[k];
        else
          word |= mask1[k];
      }
      b[j] = word;
      for (j++; j<j1; j++){
        word = b[j];
        for(k=0 ;k<BITS; k++){
          if (l==FALSE)
            word &= mask0[k];
          else
            word |= mask1[k];
        }
        b[j] = word;
      }
      k = 0;
    }
    if (j==j1 && k<=k1){
      word = b[j];
      for(; k<=k1 ;k++){
        if (l==FALSE)
          word &= mask0[k];
        else
          word |= mask1[k];
      }
      b[j] = word;
    }
}


// } --- transfer between bit and logical for certain range of b ---


// { --- transfer from bit to which for certain range of b ---

static void bit_which_positive(bitint *b, int *l, int from, int to
                          , int offset // shifts which by offset
){
  register int i=from + offset;
  from--;
  to--;
  register bitint word;
  register int h=0;
  register int k=from%BITS;
  register int j=from/BITS;
  register int k1=to%BITS;
  register int j1=to/BITS;
  if (j<j1){
    word = b[j];
    for(; k<BITS; k++){
      if (word & mask1[k])
        l[h++] = i;
      i++;
    }
    for (j++; j<j1; j++){
      word = b[j];
      for(k=0 ;k<BITS; k++){
        if (word & mask1[k])
          l[h++] = i;
        i++;
      }
    }
    k=0;
  }
  if (j==j1 && k<=k1){
    word = b[j];
    for(; k<=k1 ;k++){
      if (word & mask1[k])
        l[h++] = i;
      i++;
    }
  }
}


static void bit_which_negative(bitint *b, int *l, int from, int to){
  register int i= -to;
  from--;
  to--;
  register bitint word;
  register int h=0;
  register int k0=from%BITS;
  register int j0=from/BITS;
  register int k=to%BITS;
  register int j=to/BITS;
  if (j>j0){
    word = b[j];
    for(; k>=0; k--){
      if (!(word & mask1[k]))
        l[h++] = i;
      i++;
    }
    for (j--; j>j0; j--){
      word = b[j];
      for(k=LASTBIT ;k>=0 ;k--){
        if (!(word & mask1[k]))
          l[h++] = i;
        i++;
      }
    }
    k = LASTBIT;
  }
  if (j==j0 && k>=k0){
    word = b[j];
    for( ;k>=k0 ;k--){
      if (!(word & mask1[k]))
        l[h++] = i;
      i++;
    }
  }
}

// } --- transfer from bit to which for certain range of b ---


// { --- transfer between bit and logical for certain subscripts ---

// extract at positive unsorted subscripts i
// skip over ZEROs
// NAs and out of range (and negative) mapped to NA
static int bit_extract_unsorted(bitint *b, int nb, int *i, int ni, int *l){
  register int ii, il, ib, j, k;
  for (ii=0,il=0; ii<ni; ii++){
    if (i[ii] > 0){ // positive = neither NA nor negative nor zero
      ib = i[ii] - 1;
      if (ib<nb){  // in range
        j = ib/BITS;
        k = ib%BITS;
        l[il++] = b[j] & mask1[k] ? 1 : 0;
      }else{
        l[il++] = NA_INTEGER;
      }
    }else if (i[ii] < 0){ // either NA or negative, skip over zero
      l[il++] = NA_INTEGER;
    }
  }
  return il;
}

// extract all but at negative sorted subscripts i
// 'all' never includes ZERO or NA
// using jb and w avoids multiplication
// i is one-based and compared to w which is zero-based
// instead of comparing -i[ni]-1 against w
// we maintain jb and w negative using the following transformations
// while loop transformation (where i<0 and w>0)
//   while( (-i[ni]-1) < w )
//   while( (-i[ni]) < (w+1) )
//   while( i[ni] > -w-1 )
//   which means exclusion if
//   i[ni] = -w-1
//   or inclusion if 
//   i[ni] < -w-1
//   instead of inclusion if -i[ni]-1) != w
// hence when defining w = -w-1
//   while( i[ni] > w) )
//   which means exclusion if
//   i[ni] = w
//   or inclusion if 
//   i[ni] < w
static int bit_extract_but_sorted(bitint *b, int nb, int *i, int ni, int *l){
  register int jb, k, w, il=0;
  int j, n = nb/BITS;
  ni--; // serves as index into i 
  for (j=0,jb=-1; j<n; j++,jb-=BITS){
    for (k=0; k<BITS; k++){
      w = jb-k; 
      while( i[ni] > w ){
        if (ni>0)
          ni--;
        else 
          goto finmain;
      }
      // filter transformation
      if ( i[ni] < w){
        l[il++] = b[j] & mask1[k] ? 1 : 0;
      }
    }
  }
  n = nb%BITS;
  k=0;
  
  for (; k<n; k++){
    w = jb-k ;
    while( i[ni] > w ){
      if (ni>0)
        ni--;
      else 
        goto finrest;
    }
    if ( i[ni] < w){
      l[il++] = b[j] & mask1[k] ? 1 : 0;
    }
  }
  return il;
  
  finmain:
    for (; k<BITS; k++){
      l[il++] = b[j] & mask1[k] ? 1 : 0;
    }
    for (++j; j<n; j++){
      for (k=0; k<BITS; k++){
        l[il++] = b[j] & mask1[k] ? 1 : 0;
      }
    }
    n = nb%BITS;
  k = 0;
  
  finrest:
    for (; k<n; k++){
      l[il++] = b[j] & mask1[k] ? 1 : 0;
    }
    
    return il;
}


// replace at positive unsorted subscripts i
// skip over ZEROs and NAs (and negative)
// there is never positive out of range because vector was extended
static void bit_replace_unsorted(bitint *b, int *i, int ni, int *l){
  register int ii, il, ib, j, k;
  for (ii=0,il=0; ii<ni; ii++){
    if (i[ii]>0){ /* and != NA_INTEGER */
      ib = i[ii] - 1;
      j = ib/BITS;
      k = ib%BITS;
      if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
      ++il;
    }
  }
}

// dito but l is allowed to be shorter than i
static void bit_replace_unsorted_recycle(bitint *b, int *i, int ni, int *l, int nl){
  register int ii, il, ib, j, k;
  for (ii=0,il=0; ii<ni; ii++){
    if (i[ii]>0){
      ib = i[ii] - 1;
      j = ib/BITS;
      k = ib%BITS;
      if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
    }
    if (++il>=nl)
      il -= nl;  // recycle l
  }
}

// dito but with scalar l
static void bit_replace_unsorted_one(bitint *b, int *i, int ni, int l){
  register int ii, ib, j, k;
  if (l==NA_INTEGER)
    l = FALSE;
  for (ii=0; ii<ni; ii++){
    if (i[ii]>0){
      ib = i[ii] - 1;
      j = ib/BITS;
      k = ib%BITS;
      if (l==FALSE)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
    }
  }
}

// replace at all but negative sorted subscripts i
// there is never ZERO or NA or positive out of range
// only neg may contain out of range but needs no checking
static void bit_replace_but_sorted(bitint *b, int nb, int *i, int ni, int *l){
  register int jb, k, w, il=0;
  int j, n = nb/BITS;
  ni--; // serves as index into i 
  for (j=0,jb=-1; j<n; j++,jb-=BITS){
    for (k=0; k<BITS; k++){
      w = jb-k; 
      while( i[ni] > w ){
        if (ni>0)
          ni--;
        else 
          goto finmain;
      }
      if ( i[ni] < w){
        if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
          b[j] &= mask0[k];
        else
          b[j] |= mask1[k];
        il++;
      }
    }
  }
  n = nb%BITS;
  k=0;
  
  for (; k<n; k++){
    w = jb-k; 
    while( i[ni] > w ){
      if (ni>0)
        ni--;
      else 
        goto finrest;
    }
    if ( i[ni] < w){
      if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
      il++;
    }
  }
  return;
  
  finmain:
    for (; k<BITS; k++){
      if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
      il++;
    }
    for (++j; j<n; j++){
      for (k=0; k<BITS; k++){
        if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
          b[j] &= mask0[k];
        else
          b[j] |= mask1[k];
        il++;
      }
    }
    n = nb%BITS;
    k = 0;
  
  finrest:
    for (; k<n; k++){
      if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
      il++;
    }
    
    return;
}



// replace at all but negative sorted subscripts i
// there is never ZERO or NA or positive out of range
// only neg may contain out of range but needs no checking
static void bit_replace_but_sorted_recycle(bitint *b, int nb, int *i, int ni, int *l, int nl){
  register int jb, k, w, il=0;
  int j, n = nb/BITS;
  ni--; // serves as index into i 
  for (j=0,jb=-1; j<n; j++,jb-=BITS){
    for (k=0; k<BITS; k++){
      w = jb-k; 
      while( i[ni] > w ){
        if (ni>0)
          ni--;
        else 
          goto finmain;
      }
      if ( i[ni] < w){
        //Rprintf("1 main j=%d k=%d w=%d il=%d\n", j, k, w, il);
        if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
          b[j] &= mask0[k];
        else
          b[j] |= mask1[k];
        if (++il>=nl)
          il -= nl;  // recycle l
      }
    }
  }
  n = nb%BITS;
  k=0;
  
  for (; k<n; k++){
    w = jb-k; 
    while( i[ni] > w ){
      if (ni>0)
        ni--;
      else 
        goto finrest;
    }
    if ( i[ni] < w){
      //Rprintf("1 rest j=%d k=%d w=%d il=%d\n", j, k, w, il);
      if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
      if (++il>=nl)
        il -= nl;  // recycle l
    }
  }
  return;
  
  finmain:
    for (; k<BITS; k++){
      //Rprintf("2 pre j=%d k=%d w=%d il=%d\n", j, k, w, il);
      if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
      if (++il>=nl)
        il -= nl;  // recycle l
    }
    for (++j; j<n; j++){
      for (k=0; k<BITS; k++){
        //Rprintf("2 main j=%d k=%d w=%d il=%d\n", j, k, w, il);
        if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
          b[j] &= mask0[k];
        else
          b[j] |= mask1[k];
        if (++il>=nl)
          il -= nl;  // recycle l
      }
    }
    n = nb%BITS;
    k = 0;
  
  finrest:
    for (; k<n; k++){
      //Rprintf("2 post j=%d k=%d w=%d il=%d\n", j, k, w, il);
      if (l[il]==FALSE || l[il]==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
      if (++il>=nl)
        il -= nl;  // recycle l
    }
    
    return;
}


// replace at all but negative sorted subscripts i
// there is never ZERO or NA or positive out of range
// only neg may contain out of range but needs no checking
static void bit_replace_but_sorted_one(bitint *b, int nb, int *i, int ni, int l){
  register int jb, k, w;
  int j, n = nb/BITS;
  ni--; // serves as index into i 
  for (j=0,jb=-1; j<n; j++,jb-=BITS){
    for (k=0; k<BITS; k++){
      w = jb-k; 
      while( i[ni] > w ){
        if (ni>0)
          ni--;
        else 
          goto finmain;
      }
      if ( i[ni] < w){
        if (l==FALSE || l==NA_INTEGER)  // testing both allows l to be integer (not only logical)
          b[j] &= mask0[k];
        else
          b[j] |= mask1[k];
      }
    }
  }
  n = nb%BITS;
  k=0;
  
  for (; k<n; k++){
    w = jb-k; 
    while( i[ni] > w ){
      if (ni>0)
        ni--;
      else 
        goto finrest;
    }
    if ( i[ni] < w){
      if (l==FALSE || l==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
    }
  }
  return;
  
  finmain:
    for (; k<BITS; k++){
      if (l==FALSE || l==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
    }
    for (++j; j<n; j++){
      for (k=0; k<BITS; k++){
        if (l==FALSE || l==NA_INTEGER)  // testing both allows l to be integer (not only logical)
          b[j] &= mask0[k];
        else
          b[j] |= mask1[k];
      }
    }
    n = nb%BITS;
    k = 0;
  
  finrest:
    for (; k<n; k++){
      if (l==FALSE || l==NA_INTEGER)  // testing both allows l to be integer (not only logical)
        b[j] &= mask0[k];
      else
        b[j] |= mask1[k];
    }
    
    return;
}




// { --- special case for bit_rangediff ---
// allows y outside low high and NAs

// set all bits in low-high at y-low to TRUE
static void bit_rangediff_int2bit_lr(int low, int high, int *y, int ny, bitint *b){
  register int i, x, j, k;
  for (i=0; i<ny; i++){
    x = y[i];
    if (x != NA_INTEGER && low <= x && x <= high){
      j = (x-low)/BITS;
      k = (x-low)%BITS;
      if (~b[j] & mask1[k]){
        b[j] |= mask1[k];
      }
    }
  }
}

// set all bits in low-high at high-y to TRUE
static void bit_rangediff_int2bit_rl(int low, int high, int *y, int ny, bitint *b){
  register int i, x, j, k;
  for (i=0; i<ny; i++){
    x = y[i];
    if (x != NA_INTEGER && low <= x && x <= high){
      j = (high-x)/BITS;
      k = (high-x)%BITS;
      if (~b[j] & mask1[k]){
        b[j] |= mask1[k];
      }
    }
  }
}


// write from low to high values with zero-bit with original sign
static int bit_rangediff_bit2int_lr(int low, int high, bitint *b, int *ret){
  register int n, jb, j, k;
  register int u = 0;
  n = (high-low+1)/BITS;
  for (j=0,jb=0; j<n; j++,jb+=BITS){
    for (k=0; k<BITS; k++){
      if (~b[j] & mask1[k])
        ret[u++] = low+jb+k;
    }
  }
  n= (high-low+1)%BITS;
  for (k=0; k<n; k++){
    if (~b[j] & mask1[k])
      ret[u++] = low+jb+k;
  }
  return u;
}

// write from high to low values with zero-bit with original sign
static int bit_rangediff_bit2int_rl(int low, int high, bitint *b, int *ret){
  register int n, jb, j, k;
  register int u = 0;
  n = (high-low+1)/BITS;
  for (j=0,jb=0; j<n; j++,jb+=BITS){
    for (k=0; k<BITS; k++){
      if (~b[j] & mask1[k]){
        ret[u++] = high-(jb+k);
      }
    }
  }
  n= (high-low+1)%BITS;
  for (k=0; k<n; k++){
    if (~b[j] & mask1[k]){
      ret[u++] = high-(jb+k);
    }
  }
  return u;
}

// write from low to high values with zero-bit with reversed sign
static int bit_rangediff_bit2int_lr_rev(int low, int high, bitint *b, int *ret){
  register int n, jb, j, k;
  register int u = 0;
  n = (high-low+1)/BITS;
  for (j=0,jb=0; j<n; j++,jb+=BITS){
    for (k=0; k<BITS; k++){
      if (~b[j] & mask1[k])
        ret[u++] = -low-jb-k;
    }
  }
  n= (high-low+1)%BITS;
  for (k=0; k<n; k++){
    if (~b[j] & mask1[k])
      ret[u++] = -low-jb-k;
  }
  return u;
}

// write from high to low values with zero-bit with reversed sign
static int bit_rangediff_bit2int_rl_rev(int low, int high, bitint *b, int *ret){
  register int n, jb, j, k;
  register int u = 0;
  n = (high-low+1)/BITS;
  for (j=0,jb=0; j<n; j++,jb+=BITS){
    for (k=0; k<BITS; k++){
      if (~b[j] & mask1[k])
        ret[u++] = (jb+k)-high;
    }
  }
  n= (high-low+1)%BITS;
  for (k=0; k<n; k++){
    if (~b[j] & mask1[k])
      ret[u++] = (jb+k)-high;
  }
  return u;
}

// } --- special case for bit_rangediff ---


// { --- special case for bit_sort_unique and bit_sort ---
// allows NAs in i but all i must be within bit range of offset

// set all bits at i-low to TRUE and return number of NAs
static int bit_sort_int2bit_lr(int *i, int ni, int low, bitint *b){
  register int ii, ib, j, k, sumNA=0;
  for (ii=0; ii<ni; ii++){
    if (i[ii] == NA_INTEGER){
      sumNA++;
    }else{
      ib = i[ii] - low;
      j = ib/BITS;
      k = ib%BITS;
      b[j] |= mask1[k];
    }
  }
  return sumNA;
}

// set all bits but at high-i to TRUE and return number of NAs
static int bit_sort_int2bit_rl(int *i, int ni, int high, bitint *b){
  register int ii, ib, j, k, sumNA=0;
  for (ii=0; ii<ni; ii++){
    if (i[ii] == NA_INTEGER){
      sumNA++;
    }else{
      ib = high - i[ii];
      j = ib/BITS;
      k = ib%BITS;
      b[j] |= mask1[k];
    }
  }
  return sumNA;
}


// write all TRUE bit positions+offset to i
static int bit_sort_bit2int_lr(bitint *b, int nb, int low, int *i){
  register int j, jb, k, ii=0, n= nb/BITS;
  for (j=0,jb=0; j<n; j++,jb+=BITS){
    for (k=0; k<BITS; k++){
      if (b[j] & mask1[k])
        i[ii++] = low + jb + k;
    }
  }
  n= nb%BITS;
  for (k=0; k<n; k++){
    if (b[j] & mask1[k])
      i[ii++] = low + jb + k;
  }
  return ii;
}


static int bit_sort_bit2int_rl(bitint *b, int nb, int high, int *i){
  register int j, jb, k, ii=0, n= nb/BITS;
  for (j=0,jb=0; j<n; j++,jb+=BITS){
    for (k=0; k<BITS; k++){
      if (b[j] & mask1[k])
        i[ii++] = high - (jb + k);
    }
  }
  n= nb%BITS;
  for (k=0; k<n; k++){
    if (b[j] & mask1[k]){
      i[ii++] = high - (jb + k);
    }
  }
  return ii;
}


// receives integers in x and returns sorted in y // [[Rcpp::export]]
// while making use of b
static int *bit_sort(bitint *b, int nb, int offset, int ni, int *x, int*y, int depth){
  int ib, r, w, j, k, n= nb/BITS;
  int *z;
  // walk front to back and check bit values of x-offset
  // if they are FALSE do nothing but set x-offset to TRUE
  // if they are TRUE we have a duplicate and write the duplicate from front to back
  // (yes, overwriting values of x, but those can be recovered fro the bit vector)
  for (r=0,w=0;r<ni;r++){
    ib = x[r] - offset;
    j = ib/BITS;
    k = ib%BITS;
    if (b[j] & mask1[k]){
      x[w++] = x[r];
    }else{
      b[j] |= mask1[k];
    }
  }
  // now we have w unsorted elements in 0..(w-1)
  // now restore the sorted set of nb-w values from the bit vector
  // in positions w..(ni-1)
  // this is like bit_sort_bit2int(b, ni-w, offset, x+w);
  // but freeing the bit vector for further use
  z = x+w; // first position to write to
  r = 0;   // now used as write index
  for (j=0,ib=0; j<n; j++,ib+=BITS){
    for (k=0; k<BITS; k++){
      if (b[j] & mask1[k]){
        b[j] &= mask0[k];
        z[r++] = offset + ib + k;
      }
    }
  }
  n= nb%BITS;
  for (k=0; k<n; k++){
    if (b[j] & mask1[k]){
      b[j] &= mask0[k];
      z[r++] = offset + ib + k;
    }
  }
  // now recursively sort the w unsorted elements
  // and merge the two sorted sequences
  if (depth<=1 || w < INSERTIONSORT_LIMIT){
    // sorting in-place with three-way quicksort 
    // because we have duplicates !!
    int_quicksort3(x, 0, w-1);
    int_merge_union_all(x, w, x+w, ni-w, y);
    return y;
  }else{
    // sorting to y
    z = bit_sort(b, nb, offset, w, x, y, depth-1);
    if (z==x){
      // merge to y
      int_merge_union_all(x, w, x+w, ni-w, y);
      return y;
    }else{
      // merging inplace to x while reading from x and y
      // int_merge_union_all is stable, 
      // hence first takes from the first vector
      // hence can be used here
      // xx OPTIMIZATION OPPORTUNITY: theoretically we can stop merging as soon as y[0..(w-1)] is exhausted
      //    however, presortedness is unlikely to be the case here
      //    since the w are a (duplicate) sample of ni-w
      int_merge_union_all(y, w, x+w, ni-w, x);
      return x;
    }
  }
}


// returns all positions where i-offset is not duplicated
// treat NA like all other values (except for the fact that it is not mapped into the bit vector)
static int bit_unique_compareNA(int *i, int ni, int offset, bitint *b, int *ret){
  register int ii, ib, j, k, u=0, hasNA=FALSE;
  for (ii=0; ii<ni; ii++){
    if (i[ii] == NA_INTEGER){
      if (!hasNA){
        hasNA = TRUE;
        ret[u++] = NA_INTEGER;
      }
    }else{
      ib = i[ii] - offset;
      j = ib/BITS;
      k = ib%BITS;
      if (!(b[j] & mask1[k])){
        ret[u++] = i[ii];
        b[j] |= mask1[k];
      }
    }
  }
  return u;
}

// returns all positions where i-offset is not duplicated
// NAs are never marked as duplicated and hence always returned in the unique set
static int bit_unique_incomparableNA(int *i, int ni, int offset, bitint *b, int *ret){
  register int ii, ib, j, k, u=0;
  for (ii=0; ii<ni; ii++){
    if (i[ii] == NA_INTEGER){
      ret[u++] = NA_INTEGER;
    }else{
      ib = i[ii] - offset;
      j = ib/BITS;
      k = ib%BITS;
      if (!(b[j] & mask1[k])){
        ret[u++] = i[ii];
        b[j] |= mask1[k];
      }
    }
  }
  return u;
}

// returns all positions where i-offset is not duplicated
// NAs are always marked as duplicated and hence never returned in the unique set
static int bit_unique_removeNA(int *i, int ni, int offset, bitint *b, int *ret){
  register int ii, ib, j, k, u=0;
  for (ii=0; ii<ni; ii++){
    if (i[ii] != NA_INTEGER){
      ib = i[ii] - offset;
      j = ib/BITS;
      k = ib%BITS;
      if (!(b[j] & mask1[k])){
        ret[u++] = i[ii];
        b[j] |= mask1[k];
      }
    }
  }
  return u;
}

// set all bits where i-offset is duplicated
// treat NA like all other values (except for the fact that it is not mapped into the bit vector)
static void bit_duplicated_compareNA(int *i, int ni, int offset, bitint *b, bitint *ret){
  register int ii, ib, j, k, hasNA = FALSE;
  for (ii=0; ii<ni; ii++){
    if (i[ii] == NA_INTEGER){
      if (hasNA){
        j = ii/BITS;
        k = ii%BITS;
        ret[j] |= mask1[k];
      }else
        hasNA = TRUE;
    }else{
      ib = i[ii] - offset;
      j = ib/BITS;
      k = ib%BITS;
      if (b[j] & mask1[k]){
        j = ii/BITS;
        k = ii%BITS;
        ret[j] |= mask1[k];
      }else
        b[j] |= mask1[k];
    }
  }
}


// set all bits where i-offset is duplicated
// NAs are never marked as duplicated and hence always returned in the unique set
static void bit_duplicated_incomparableNA(int *i, int ni, int offset, bitint *b, bitint *ret){
  register int ii, ib, j, k;
  for (ii=0; ii<ni; ii++){
    if (i[ii] != NA_INTEGER){
      ib = i[ii] - offset;
      j = ib/BITS;
      k = ib%BITS;
      if (b[j] & mask1[k]){
        j = ii/BITS;
        k = ii%BITS;
        ret[j] |= mask1[k];
      }else
        b[j] |= mask1[k];
    }
  }
}


// set all bits where i-offset is duplicated
// NAs are always marked as duplicated and hence never returned in the unique set
static void bit_duplicated_removeNA(int *i, int ni, int offset, bitint *b, bitint *ret){
  register int ii, ib, j, k;
  for (ii=0; ii<ni; ii++){
    if (i[ii] == NA_INTEGER){
      j = ii/BITS;
      k = ii%BITS;
      ret[j] |= mask1[k];
    }else{
      ib = i[ii] - offset;
      j = ib/BITS;
      k = ib%BITS;
      if (b[j] & mask1[k]){
        j = ii/BITS;
        k = ii%BITS;
        ret[j] |= mask1[k];
      }else
        b[j] |= mask1[k];
    }
  }
}


static int bit_anyDuplicated_compareNA(int *i, int ni, int offset, bitint *b){
  register int ii, ib, j, k, hasNA=FALSE, s=0;
  for (ii=0; ii<ni; ii++){
    if (i[ii] == NA_INTEGER){
      if (hasNA){
        s=ii+1; // one-based position of first NA duplicate
        break;
      }else
        hasNA = TRUE;
    }else{
      ib = i[ii] - offset;
      j = ib/BITS;
      k = ib%BITS;
      if (b[j] & mask1[k]){
        s=ii+1; // one-based position of first non-NA duplicate
        break;
      }else
        b[j] |= mask1[k];
    }
  }
  return s;
}

// check whether any i-offset is duplicated
static int bit_anyDuplicated_incomparableNA(int *i, int ni, int offset, bitint *b){
  register int ii, ib, j, k, s=0;
  for (ii=0; ii<ni; ii++){
    if (i[ii] != NA_INTEGER){
      ib = i[ii] - offset;
      j = ib/BITS;
      k = ib%BITS;
      if (b[j] & mask1[k]){
        s=ii+1; // one-based position of first non-NA duplicate
        break;
      }else
        b[j] |= mask1[k];
    }
  }
  return s;
}

static int bit_anyDuplicated_removeNA(int *i, int ni, int offset, bitint *b){
  register int ii, ib, j, k, s=0;
  for (ii=0; ii<ni; ii++){
    if (i[ii] == NA_INTEGER){
      s=ii+1; // one-based position of first NA
      break;
    }else{
      ib = i[ii] - offset;
      j = ib/BITS;
      k = ib%BITS;
      if (b[j] & mask1[k]){
        s=ii+1; // one-based position of first non-NA duplicate
        break;
      }else
        b[j] |= mask1[k];
    }
  }
  return s;
}

// return number of bits where i-offset is duplicated and the number of NAs
static void bit_sumDuplicatedNA(int *i, int ni, int offset, bitint *b, int *ret){
  register int ii, ib, j, k, s=0, sumNA=0;
  for (ii=0; ii<ni; ii++){
    if (i[ii] == NA_INTEGER){
      sumNA++;
    }else{
      ib = i[ii] - offset;
      j = ib/BITS;
      k = ib%BITS;
      if (b[j] & mask1[k])
        s++;
      else
        b[j] |= mask1[k];
    }
  }
  ret[0] = s;
  ret[1] = sumNA;
}


// match x in t by first bitmapping t to trange and then verifying each x in trange
// table has NAs and each NA is mapped to TRUE
static void bit_in_table_NA(int *t, int nt, int *x, int nx, int tmin, int tmax, bitint *b, bitint *ret){
  register int i, n = nx/BITS;
  register int bt, jt, kt;
  register int bx, jx, kx;
  // mark table in bits
  for (i=0; i<nt; i++){
    if (t[i] != NA_INTEGER){
      bt = t[i] - tmin;
      jt = bt/BITS;
      kt = bt%BITS;
      b[jt] |= mask1[kt];
    }
  }
  for (jx=0,i=0; jx<n; jx++){
    for (kx=0; kx<BITS; kx++,i++){
      bx = x[i];
      if (bx == NA_INTEGER){
        ret[jx] |= mask1[kx];
      }else if (tmin<=bx && bx<=tmax){
        bx -= tmin;
        jt = bx/BITS;
        kt = bx%BITS;
        if (b[jt] & mask1[kt])
          ret[jx] |= mask1[kx];
      }
    }
  }
  for (kx=0; i<nx; kx++,i++){
    bx = x[i];
    if (bx == NA_INTEGER){
      ret[jx] |= mask1[kx];
    }else if (tmin<=bx && bx<=tmax){
      bx -= tmin;
      jt = bx/BITS;
      kt = bx%BITS;
      if (b[jt] & mask1[kt])
        ret[jx] |= mask1[kx];
    }
  }
}

// match x in t by first bitmapping t to trange and then verifying each x in trange
// table has no NAs and each NA is mapped to FALSE (do nothing)
static void bit_in_table_NoNA(int *t, int nt, int *x, int nx, int tmin, int tmax, bitint *b, bitint *ret){
  register int i, n = nx/BITS;
  register int bt, jt, kt;
  register int bx, jx, kx;
  // mark table in bits
  for (i=0; i<nt; i++){
    bt = t[i] - tmin;
    jt = bt/BITS;
    kt = bt%BITS;
    b[jt] |= mask1[kt];
  }
  for (jx=0,i=0; jx<n; jx++){
    for (kx=0; kx<BITS; kx++,i++){
      bx = x[i];
      if (bx != NA_INTEGER && tmin<=bx && bx<=tmax){
        bx -= tmin;
        jt = bx/BITS;
        kt = bx%BITS;
        if (b[jt] & mask1[kt])
          ret[jx] |= mask1[kx];
      }
    }
  }
  for (kx=0; i<nx; kx++,i++){
    bx = x[i];
    if (bx != NA_INTEGER && tmin<=bx && bx<=tmax){
      bx -= tmin;
      jt = bx/BITS;
      kt = bx%BITS;
      if (b[jt] & mask1[kt])
        ret[jx] |= mask1[kx];
    }
  }
}  

// match x in larger t by bitmapping t to xrange and then verifying x in xrange
// table has NAs and each NA is mapped to TRUE
static void bit_table_in_NA(int *t, int nt, int *x, int nx, int xmin, int xmax, bitint *b, bitint *ret){
  register int i, n = nx/BITS;
  register int bt, jt, kt;
  register int bx, jx, kx;
  register int hasna = FALSE;
  // mark table in bits until NA found
  for (i=0; i<nt; i++){
    bt = t[i];
    if (bt  == NA_INTEGER){
      hasna = TRUE;
      i++;
      break; // jump into cheaper loop
    }else if (xmin<=bt && bt<=xmax){
      bt -= xmin;
      jt = bt/BITS;
      kt = bt%BITS;
      b[jt] |= mask1[kt];
    }
  }
  // mark table in bits skipping marking NA
  for (; i<nt; i++){
    bt = t[i];
    if (bt  != NA_INTEGER  && xmin<=bt && bt<=xmax){
      bt -= xmin;
      jt = bt/BITS;
      kt = bt%BITS;
      b[jt] |= mask1[kt];
    }
  }
  for (jx=0,i=0; jx<n; jx++){
    for (kx=0; kx<BITS; kx++,i++){
      bx = x[i];
      if (bx == NA_INTEGER){
        if (hasna)
          ret[jx] |= mask1[kx];
      }else{
        bx -= xmin;
        jt = bx/BITS;
        kt = bx%BITS;
        if (b[jt] & mask1[kt])
          ret[jx] |= mask1[kx];
      }
    }
  }
  for (kx=0; i<nx; kx++,i++){
    bx = x[i];
    if (bx == NA_INTEGER){
      if (hasna)
        ret[jx] |= mask1[kx];
    }else{
      bx -= xmin;
      jt = bx/BITS;
      kt = bx%BITS;
      if (b[jt] & mask1[kt])
        ret[jx] |= mask1[kx];
    }
  }
}


static void bit_table_in_NoNA(int *t, int nt, int *x, int nx, int xmin, int xmax, bitint *b, bitint *ret){
  register int i, n = nx/BITS;
  register int bt, jt, kt;
  register int bx, jx, kx;
  // mark table in bits ignoring NA
  for (i=0; i<nt; i++){
    bt = t[i];
    if (bt  != NA_INTEGER && xmin<=bt && bt<=xmax){
      bt -= xmin;
      jt = bt/BITS;
      kt = bt%BITS;
      b[jt] |= mask1[kt];
    }
  }
  for (jx=0,i=0; jx<n; jx++){
    for (kx=0; kx<BITS; kx++,i++){
      bx = x[i];
      bx -= xmin;
      jt = bx/BITS;
      kt = bx%BITS;
      if (b[jt] & mask1[kt])
        ret[jx] |= mask1[kx];
    }
  }
  for (kx=0; i<nx; kx++,i++){
    bx = x[i];
    bx -= xmin;
    jt = bx/BITS;
    kt = bx%BITS;
    if (b[jt] & mask1[kt])
      ret[jx] |= mask1[kx];
  }
}


static int bit_union_NA(int *x, int nx, int *y, int ny, int low, bitint *b, int *ret){
  register int i;
  register int ib, j, k;
  register int hasNA = FALSE;
  register int u = 0;
  for (i=0; i<nx; i++){
    if (x[i] == NA_INTEGER){
      if (!hasNA){
        hasNA = TRUE;
        ret[u++] = NA_INTEGER;
      }
    }else{
      ib = x[i] - low;
      j = ib/BITS;
      k = ib%BITS;
      if (!(b[j] & mask1[k])){
        ret[u++] = x[i];
        b[j] |= mask1[k];
      }
    }
  }
  for (i=0; i<ny; i++){
    if (y[i] == NA_INTEGER){
      if (!hasNA){
        hasNA = TRUE;
        ret[u++] = NA_INTEGER;
      }
    }else{
      ib = y[i] - low;
      j = ib/BITS;
      k = ib%BITS;
      if (!(b[j] & mask1[k])){
        ret[u++] = y[i];
        b[j] |= mask1[k];
      }
    }
  }
  return u;
}

static int bit_union_NoNA(int *x, int nx, int *y, int ny, int low, bitint *b, int *ret){
  register int i;
  register int ib, j, k;
  register int u = 0;
  for (i=0; i<nx; i++){
    ib = x[i] - low;
    j = ib/BITS;
    k = ib%BITS;
    if (!(b[j] & mask1[k])){
      ret[u++] = x[i];
      b[j] |= mask1[k];
    }
  }
  for (i=0; i<ny; i++){
    ib = y[i] - low;
    j = ib/BITS;
    k = ib%BITS;
    if (!(b[j] & mask1[k])){
      ret[u++] = y[i];
      b[j] |= mask1[k];
    }
  }
  return u;
}


// the first set (x) determines the order, hence we first register the second set
static int bit_intersect_NA(int *x, int nx, int *y, int ny, int low, int high, bitint *b, int *ret){
  register int i;
  register int ib, j, k;
  register int hasNA = FALSE;
  register int u = 0;
  for (i=0; i<ny; i++){
    ib = y[i];
    if (ib == NA_INTEGER){
      if (!hasNA){
        hasNA = TRUE;
      }
    }else if (low <= ib && ib <= high){
      ib -= low;
      j = ib/BITS;
      k = ib%BITS;
      b[j] |= mask1[k];
    }
  }
  for (i=0; i<nx; i++){
    ib = x[i];
    if (ib == NA_INTEGER){
      if (hasNA){
        hasNA = FALSE;
        ret[u++] = NA_INTEGER;
      }
    }else if (low <= ib && ib <= high){
      ib -= low;
      j = ib/BITS;
      k = ib%BITS;
      if (b[j] & mask1[k]){
        ret[u++] = x[i];
        b[j] &= mask0[k];
      }
    }
  }
  return u;
}

static int bit_intersect_NoNA(int *x, int nx, int *y, int ny, int low, int high, bitint *b, int *ret){
  register int i;
  register int ib, j, k;
  register int u = 0;
  for (i=0; i<ny; i++){
    ib = y[i];
    if (low <= ib && ib <= high){
      ib -= low;
      j = ib/BITS;
      k = ib%BITS;
      b[j] |= mask1[k];
    }
  }
  for (i=0; i<nx; i++){
    ib = x[i];
    if (low <= ib && ib <= high){
      ib -= low;
      j = ib/BITS;
      k = ib%BITS;
      if (b[j] & mask1[k]){
        ret[u++] = x[i];
        b[j] &= mask0[k];
      }
    }
  }
  return u;
}


// the first set (x) determines the order, hence we first register the second set
static int bit_setdiff_NA(int *x, int nx, int *y, int ny, int low, int high, bitint *b, int *ret){
  register int i;
  register int ib, j, k;
  register int hasNA = FALSE;
  register int u = 0;
  for (i=0; i<ny; i++){
    ib = y[i];
    if (ib == NA_INTEGER){
      if (!hasNA){
        hasNA = TRUE;
      }
    }else if (low <= ib && ib <= high){
      ib -= low;
      j = ib/BITS;
      k = ib%BITS;
      b[j] |= mask1[k];
    }
  }
  for (i=0; i<nx; i++){
    ib = x[i];
    if (ib == NA_INTEGER){
      if (!hasNA){
        hasNA = TRUE;
        ret[u++] = NA_INTEGER;
      }
    }else{
      ib -= low;
      j = ib/BITS;
      k = ib%BITS;
      if (~b[j] & mask1[k]){
        ret[u++] = x[i];
        b[j] |= mask1[k];
      }
    }
  }
  return u;
}

static int bit_setdiff_NoNA(int *x, int nx, int *y, int ny, int low, int high, bitint *b, int *ret){
  register int i;
  register int ib, j, k;
  register int hasNA = FALSE;
  register int u = 0;
  for (i=0; i<ny; i++){
    ib = y[i];
    if (ib != NA_INTEGER && low <= ib && ib <= high){
      ib -= low;
      j = ib/BITS;
      k = ib%BITS;
      b[j] |= mask1[k];
    }
  }
  for (i=0; i<nx; i++){
    ib = x[i];
    if (ib == NA_INTEGER){
      if (!hasNA){
        hasNA = TRUE;
        ret[u++] = NA_INTEGER;
      }
    }else{
      ib -= low;
      j = ib/BITS;
      k = ib%BITS;
      if (~b[j] & mask1[k]){
        ret[u++] = x[i];
        b[j] |= mask1[k];
      }
    }
  }
  return u;
}


static int bit_symdiff_NA(int *x, int nx, int *y, int ny, int low, int xhasNA, int yhasNA, bitint *bx, bitint *by, int *ret){
  register int i;
  register int j, k;
  register int u = 0;
  register int hasNA = (xhasNA != yhasNA) ? TRUE : FALSE;
  // register x in bx and y in by (except for NAs which we know already)
  if (yhasNA){ 
    for (i=0; i<ny; i++){
      if (y[i] != NA_INTEGER){
        j = (y[i] - low)/BITS;
        k = (y[i] - low)%BITS;
        by[j] |= mask1[k];
      }
    }
  }else{
    for (i=0; i<ny; i++){
      j = (y[i] - low)/BITS;
      k = (y[i] - low)%BITS;
      by[j] |= mask1[k];
    }
  }
  if (xhasNA){ 
    for (i=0; i<nx; i++){
      if (x[i] != NA_INTEGER){
        j = (x[i] - low)/BITS;
        k = (x[i] - low)%BITS;
        bx[j] |= mask1[k];
      }
    }
  }else{
    for (i=0; i<nx; i++){
      j = (x[i] - low)/BITS;
      k = (x[i] - low)%BITS;
      bx[j] |= mask1[k];
    }
  }
  // now scan x and y using by and bx
  if (xhasNA){ 
    for (i=0; i<nx; i++){
      if (x[i] == NA_INTEGER){
        if (hasNA){
          ret[u++] = NA_INTEGER;
          hasNA = FALSE;
        }
      }else{
        j = (x[i] - low)/BITS;
        k = (x[i] - low)%BITS;
        if (!(by[j] & mask1[k])){
          by[j] |= mask1[k];
          ret[u++] = x[i];
        }
      }
    }
  }else{
    for (i=0; i<nx; i++){
      j = (x[i] - low)/BITS;
      k = (x[i] - low)%BITS;
      if (!(by[j] & mask1[k])){
        by[j] |= mask1[k];
        ret[u++] = x[i];
      }
    }
  }
  
  if (yhasNA){ 
    for (i=0; i<ny; i++){
      if (y[i] == NA_INTEGER){
        if (hasNA){
          ret[u++] = NA_INTEGER;
          hasNA = FALSE;
        }
      }else{
        j = (y[i] - low)/BITS;
        k = (y[i] - low)%BITS;
        if (!(bx[j] & mask1[k])){
          bx[j] |= mask1[k];
          ret[u++] = y[i];
        }
      }
    }
  }else{
    for (i=0; i<ny; i++){
      j = (y[i] - low)/BITS;
      k = (y[i] - low)%BITS;
      if (!(bx[j] & mask1[k])){
        bx[j] |= mask1[k];
        ret[u++] = y[i];
      }
    }
  }
  return u;
}


static int bit_symdiff_NoNA(int *x, int nx, int *y, int ny, int low, bitint *bx, bitint *by, int *ret){
  register int i;
  register int j, k;
  register int u = 0;
  // register x in bx and y in by
  for (i=0; i<ny; i++){
    j = (y[i] - low)/BITS;
    k = (y[i] - low)%BITS;
    by[j] |= mask1[k];
  }
  for (i=0; i<nx; i++){
    j = (x[i] - low)/BITS;
    k = (x[i] - low)%BITS;
    bx[j] |= mask1[k];
  }
  // now scan x and y using by and bx
  for (i=0; i<nx; i++){
    j = (x[i] - low)/BITS;
    k = (x[i] - low)%BITS;
    if (!(by[j] & mask1[k])){
      by[j] |= mask1[k];
      ret[u++] = x[i];
    }
  }
  for (i=0; i<ny; i++){
    j = (y[i] - low)/BITS;
    k = (y[i] - low)%BITS;
    if (!(bx[j] & mask1[k])){
      bx[j] |= mask1[k];
      ret[u++] = y[i];
    }
  }
  return u;
}



static int bit_setequal_NA(int *x, int nx, int *y, int ny, int low, int high, bitint *bx, bitint *by){
  register int i;
  register int ib, j, k;
  for (i=0; i<nx; i++){
    ib = x[i];
    if (ib != NA_INTEGER){
      ib -= low;
      j = ib/BITS;
      k = ib%BITS;
      bx[j] |= mask1[k];
    }
  }
  for (i=0; i<ny; i++){
    ib = y[i];
    if (ib != NA_INTEGER){
      ib -= low;
      j = ib/BITS;
      k = ib%BITS;
      by[j] |= mask1[k];
    }
  }
  nx = (high-low+1);
  nx = nx/BITS + (nx%BITS ? 1 : 0);
  for (i=0; i<nx; i++){
    if (bx[i]!=by[i])
      return(FALSE);
  }
  return TRUE;
}

static int bit_setequal_NoNA(int *x, int nx, int *y, int ny, int low, int high, bitint *bx, bitint *by){
  register int i;
  register int ib, j, k;
  for (i=0; i<nx; i++){
    ib = x[i] - low;
    j = ib/BITS;
    k = ib%BITS;
    bx[j] |= mask1[k];
  }
  for (i=0; i<ny; i++){
    ib = y[i] - low;
    j = ib/BITS;
    k = ib%BITS;
    by[j] |= mask1[k];
  }
  nx = (high-low+1);
  nx = nx/BITS + (nx%BITS ? 1 : 0);
  for (i=0; i<nx; i++){
    if (bx[i]!=by[i])
      return(FALSE);
  }
  return TRUE;
}




// } --- special case for bit_sort ---

// } --- transfer between bit and logical for certain subscripts ---




static void bit_not(bitint *b, int n){
  register int i, ni= n/BITS;
  int k = n%BITS;
  for (i=0; i<ni; i++){
    b[i] = ~b[i];
  }
  if (k){
    // last bitint
    b[i] = ~b[i];
    // set unused bits to zero
    while(k<BITS)
      b[i] &= mask0[k++];
  }
}


static void bit_and(bitint *b1, bitint *b2, bitint *ret, int n){
  register int i, ni= n/BITS;
  int k = n%BITS;
  for (i=0; i<ni; i++){
    ret[i] = b1[i] & b2[i];
  }
  if (k){
    // last bitint
    ret[i] = b1[i] & b2[i];
    // set unused bits to zero
    while(k<BITS)
      ret[i] &= mask0[k++];
  }
}

static void bit_or(bitint *b1, bitint *b2, bitint *ret, int n){
  register int i, ni= n/BITS;
  int k = n%BITS;
  for (i=0; i<ni; i++){
    ret[i] = b1[i] | b2[i];
  }
  if (k){
    // last bitint
    ret[i] = b1[i] | b2[i];
    // set unused bits to zero
    while(k<BITS)
      ret[i] &= mask0[k++];
  }
}

static void bit_xor(bitint *b1, bitint *b2, bitint *ret, int n){
  register int i, ni= n/BITS;
  int k = n%BITS;
  for (i=0; i<ni; i++){
    ret[i] = b1[i] ^ b2[i];
  }
  if (k){
    // last bitint
    ret[i] = b1[i] ^ b2[i];
    // set unused bits to zero
    while(k<BITS)
      ret[i] &= mask0[k++];
  }
}

static void bit_equal(bitint *b1, bitint *b2, bitint *ret, int n){
  register int i, ni= n/BITS;
  int k = n%BITS;
  for (i=0; i<ni; i++){
    ret[i] = ~(b1[i] ^ b2[i]);
  }
  if (k){
    // last bitint
    ret[i] = ~(b1[i] ^ b2[i]);
    // set unused bits to zero
    while(k<BITS)
      ret[i] &= mask0[k++];
  }
}


static int bit_sum(bitint *b, int from, int to){
  from--;
  to--;
  register bitint word;
  register int s=0;
  register int k=from%BITS;
  register int j=from/BITS;
  register int k1=to%BITS;
  register int j1=to/BITS;
  if (j<j1){
    word = b[j];
    for(; k<BITS; k++){
      if (word & mask1[k])
        s++;
    }
    for (j++; j<j1; j++){
      word = b[j];
      for(k=0; k<BITS; k++){
        if (word & mask1[k])
          s++;
      }
    }
    k=0;
  }
  if (j==j1 && k<=k1){
    word = b[j];
    for(; k<=k1; k++){
      if (word & mask1[k])
        s++;
    }
  }
  return s;
}


static int bit_all(bitint *b, int from, int to){
  from--;
  to--;
  register bitint word;
  register int k=from%BITS;
  register int j=from/BITS;
  register int k1=to%BITS;
  register int j1=to/BITS;
  if (j<j1){
    word = b[j];
    for(; k<BITS; k++){
      if (!(word & mask1[k]))
        return 0;
    }
    for (j++; j<j1; j++){
      if(~(b[j]))
        return 0;
    }
    k=0;
  }
  if (j==j1 && k<=k1){
    word = b[j];
    for(; k<=k1; k++){
      if (!(word & mask1[k]))
        return 0;
    }
  }
  return 1;
}



static int bit_any(bitint *b, int from, int to){
  from--;
  to--;
  register bitint word;
  register int k=from%BITS;
  register int j=from/BITS;
  register int k1=to%BITS;
  register int j1=to/BITS;
  if (j<j1){
    word = b[j];
    for(; k<BITS; k++){
      if (word & mask1[k])
        return 1;
    }
    for (j++; j<j1; j++){
      if(b[j])
        return 1;
    }
    k=0;
  }
  if (j==j1 && k<=k1){
    word = b[j];
    for(; k<=k1; k++){
      if(b[j])
        return 1;
    }
  }
  return 0;
}



static int bit_min(bitint *b, int from, int to){
  from--;
  to--;
  register bitint word;
  register int k=from%BITS;
  register int j=from/BITS;
  register int k1=to%BITS;
  register int j1=to/BITS;
  if (j<j1){
    word = b[j];
    if(word){
      for(; k<BITS; k++){
        if (word & mask1[k]){
          return j*BITS + k + 1;
        }
      }
    }
    for (j++; j<j1; j++){
      word = b[j];
      if (word)
        for(k=0; k<BITS ;k++){
          if (word & mask1[k]){
            return j*BITS + k + 1;
          }
        }
    }
    k=0;
  }
  if (j==j1 && k<=k1){
    word = b[j];
    if (word)
      for(; k<=k1; k++){
        if (word & mask1[k]){
          return j*BITS+k+1;
        }
      }
  }
  
  return NA_INTEGER;
}



static int bit_max(bitint *b, int from, int to){
  from--;
  to--;
  register bitint word;
  register int k0=from%BITS;
  register int j0=from/BITS;
  register int k=to%BITS;
  register int j=to/BITS;
  if (j>j0){
    word = b[j];
    if (word){
      for(; k>=0; k--){
        if (word & mask1[k])
          return j*BITS+k+1;
      }
    }
    for (j--; j>j0; j--){
      word = b[j];
      if (word){
        for(k=LASTBIT; k>=0; k--){
          if (word & mask1[k])
            return j*BITS+k+1;
        }
      }
    }
    k=LASTBIT;
  }
  if (j==j0 && k>=k0){
    word = b[j];
    if (word){
      for(; k>=k0; k--){
        if (word & mask1[k])
          return j*BITS+k+1;
      }
    }
  }
  return NA_INTEGER;
}


/* R interface functions -------------------- */

SEXP R_bit_shiftcopy(
    SEXP bsource_  /* bit source */
, SEXP btarget_    /* bit target: assuming FALSE in the target positions and above */
, SEXP otarget_    /* offset target */
, SEXP n_          /* number of bits to copy */
){
  bitint *bsource = (bitint*) INTEGER(bsource_);
  bitint *btarget = (bitint*) INTEGER(btarget_);
  int otarget = asInteger(otarget_);
  int n = asInteger(n_);
  bit_shiftcopy(bsource, btarget, otarget, n);
  return(btarget_);
}

SEXP R_bit_reverse(
  SEXP bsource_  /* bit source */
, SEXP btarget_  /* bit target: assuming equal length and FALSE in the target positions and above */
){
  bitint *bsource = (bitint*) INTEGER(bsource_);
  bitint *btarget = (bitint*) INTEGER(btarget_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP svirt = PROTECT(getAttrib(bsource_, virt));
  SEXP sleng = PROTECT(getAttrib(svirt, leng));
  SEXP tvirt = PROTECT(getAttrib(btarget_, virt));
  SEXP tleng = PROTECT(getAttrib(tvirt, leng));
  int ns = asInteger(sleng);
  int nt = asInteger(tleng);
  UNPROTECT(6);
  if (ns!= nt)
    error("source and target must have same length in R_bit_reverse");
  bit_reverse(bsource, btarget, ns);
  return(btarget_);
}

// alters b_
SEXP R_bit_recycle(SEXP b_, SEXP r_){
  bitint *b = (bitint*) INTEGER(b_);
  bitint *r = (bitint*) INTEGER(r_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP bvirt = PROTECT(getAttrib(b_, virt));
  SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  SEXP rvirt = PROTECT(getAttrib(r_, virt));
  SEXP rleng = PROTECT(getAttrib(rvirt, leng));
  int nb = asInteger(bleng);
  int nr = asInteger(rleng);
  UNPROTECT(6);
  int dr,i,k,n;
  if (nb<nr){
    k = nb % BITS;
    n = nb / BITS;
    for (i=0; i<n; i++)
      b[i] = r[i];
    if (k){
      b[i] = r[i];
      // set unused bits to zero
      while(k<BITS)
        b[i] &= mask0[k++];
    }
  }else{
    k = nr % BITS;
    n = nr / BITS;
    for (i=0; i<n; i++)
      b[i] = r[i];
    if (k){
      b[i] = r[i];
    }
    while(nr<nb){
      dr = nb - nr;
      if (dr>nr)
        dr = nr;
      bit_shiftcopy(b, b, nr, dr);
      nr += dr;
    }
  }
  return(b_);
}


// alters l_
SEXP R_bit_get_logical(SEXP b_, SEXP l_, SEXP range_){
  bitint *b = (bitint*) INTEGER(b_);
  int *l = LOGICAL(l_);
  int *range = INTEGER(range_);
  bit_get(b, l, range[0], range[1]);
  return(l_);
}

// alters l_
SEXP R_bit_get_integer(SEXP b_, SEXP l_, SEXP range_){
  bitint *b = (bitint*) INTEGER(b_);
  int *l = INTEGER(l_);
  int *range = INTEGER(range_);
  bit_get(b, l, range[0], range[1]);
  return(l_);
}


// alters b_
SEXP R_bit_set_logical(SEXP b_, SEXP l_, SEXP range_){
  bitint *b = (bitint*) INTEGER(b_);
  int *l = LOGICAL(l_);
  int *range = INTEGER(range_);
  int nr, nl = LENGTH(l_);
  if (nl==1) 
    bit_set_one(b, l[0], range[0], range[1]);
  else if (nl == (nr = range[1] - range[0] + 1)) 
    bit_set(b, l, range[0], range[1]);
  else
    bit_set_recycle(b, l, range[0], range[1], nl);
  return(b_);
}

// alters b_
SEXP R_bit_set_integer(SEXP b_, SEXP l_, SEXP range_){
  bitint *b = (bitint*) INTEGER(b_);
  int *l = INTEGER(l_);
  int *range = INTEGER(range_);
  int nr, nl = LENGTH(l_);
  if (nl==1) 
    bit_set_one(b, l[0], range[0], range[1]);
  else if (nl == (nr = range[1] - range[0] + 1)) 
    bit_set(b, l, range[0], range[1]);
  else 
    bit_set_recycle(b, l, range[0], range[1], nl);
  return(b_);
}


SEXP R_bit_which(SEXP b_, SEXP s_, SEXP range_, SEXP negative_){
  bitint *b = (bitint*) INTEGER(b_);
  int *range = INTEGER(range_);
  int s = asInteger(s_);
  SEXP ret_;
  int *ret;
  if (asLogical(negative_)){
    // negative return
    PROTECT( ret_ = allocVector(INTSXP,s) );
    ret = INTEGER(ret_);
    bit_which_negative(b, ret, range[0], range[1]);
  }else{
    // positive return
    PROTECT( ret_ = allocVector(INTSXP,s) );
    ret = INTEGER(ret_);
    bit_which_positive(b, ret, range[0], range[1], 0);
  }
  UNPROTECT(1);
  return(ret_);
}


SEXP R_bit_sort(SEXP b_, SEXP i_, SEXP range_, SEXP na_last_, SEXP depth_){
  bitint *b = (bitint*) INTEGER(b_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP bvirt = PROTECT(getAttrib(b_, virt));
  SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  int nb = asInteger(bleng);
  UNPROTECT(4);
  int *i = INTEGER(i_);
  int *r = INTEGER(range_);
  int na_last = asLogical(na_last_);
  int ni = LENGTH(i_);
  int depth = asInteger(depth_);
  //SEXP x_;
  SEXP y_;
  //PROTECT( x_ = allocVector(INTSXP,ni) );
  PROTECT( y_ = allocVector(INTSXP,ni) );
  //int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int *z;
  int ii;
  GetRNGstate();
  if (na_last==FALSE){
    // NAs at beginning
    //for(ii=ni-1;ii>=r[2]; ii--)
    //  x[ii] = i[ii];
    z = bit_sort(b, nb, r[0], ni-r[2], i+r[2], y+r[2], depth) - r[2];
    for(ii=r[2]-1;ii>=0; ii--){
      z[ii] = NA_INTEGER;
    }
  }else{
    // NAs at end
    //for (ii=ni-r[2]-1; ii>=0; ii--)
    //  x[ii] = i[ii];
    z = bit_sort(b, nb, r[0], ni-r[2], i, y, depth);
    for(ii=ni-r[2]; ii<ni; ii++)
      z[ii] = NA_INTEGER;
  }
  PutRNGstate();
  UNPROTECT(1);
  if (z==i){
    return i_;
  }else{
    return y_;
  }
}

SEXP R_bit_sort_unique(SEXP b_, SEXP i_, SEXP range_, SEXP nalast_, SEXP decreasing_){
  bitint *b = (bitint*) INTEGER(b_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP bvirt = PROTECT(getAttrib(b_, virt));
  SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  int nb = asInteger(bleng);
  UNPROTECT(4);
  int nalast = asLogical(nalast_);
  int decreasing = asLogical(decreasing_);
  int *i = INTEGER(i_);
  int *r = INTEGER(range_);
  int ni = LENGTH(i_);
  int sumNA;
  SEXP ret_;
  int *ret;
  if (decreasing)
    sumNA = bit_sort_int2bit_rl(i, ni, r[1], b);
  else
    sumNA = bit_sort_int2bit_lr(i, ni, r[0], b);
  if (sumNA == 0 || nalast == NA_LOGICAL){
    PROTECT( ret_ = allocVector(INTSXP,ni) );
    ret = INTEGER(ret_);
    if (decreasing)
      ni = bit_sort_bit2int_rl(b, nb, r[1], ret);
    else
      ni = bit_sort_bit2int_lr(b, nb, r[0], ret);
    ret_ = Rf_lengthgets(ret_, ni);  // no REPROTECT needed as long as we leave function immediately
  }else if (nalast==TRUE){
    PROTECT( ret_ = allocVector(INTSXP,ni+1) );
    ret = INTEGER(ret_);
    if (decreasing)
      ni = bit_sort_bit2int_rl(b, nb, r[1], ret);
    else
      ni = bit_sort_bit2int_lr(b, nb, r[0], ret);
    ret[ni] = NA_INTEGER;
    ret_ = Rf_lengthgets(ret_, ni+1);  // no REPROTECT needed as long as we leave function immediately
  }else{
    PROTECT( ret_ = allocVector(INTSXP,ni+1) );
    ret = INTEGER(ret_);
    ret[0] = NA_INTEGER;
    if (decreasing)
      ni = bit_sort_bit2int_rl(b, nb, r[1], ret+1);
    else
      ni = bit_sort_bit2int_lr(b, nb, r[0], ret+1);
    ret_ = Rf_lengthgets(ret_, ni+1);  // no REPROTECT needed as long as we leave function immediately
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_rangediff(SEXP b_, SEXP rx_, SEXP y_, SEXP revx_, SEXP revy_){
  bitint *b = (bitint*) INTEGER(b_);
  // SEXP virt = PROTECT(install("virtual"));   
  // SEXP leng = PROTECT(install("Length"));
  // SEXP bvirt = PROTECT(getAttrib(b_, virt));
  // SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  // int nb = asInteger(bleng);
  // UNPROTECT(4);
  int revx = asLogical(revx_);
  int revy = asLogical(revy_);
  int *rx = INTEGER(rx_);
  int *y = INTEGER(y_);
  int nx, ny = LENGTH(y_);
  SEXP ret_;
  int *ret;
  int decreasing = 0;
  if (rx[0]>rx[1]){
    decreasing = 1;
    nx = rx[0] -  rx[1] + 1;
  }else{
    nx = rx[1] -  rx[0] + 1;
  }
  PROTECT( ret_ = allocVector(INTSXP,nx) );
  ret = INTEGER(ret_);
  if (decreasing){
    if (revx){
      if (revy){
        bit_rangediff_int2bit_lr(rx[1], rx[0], y, ny, b);
        nx = bit_rangediff_bit2int_lr_rev(rx[1], rx[0], b, ret);
      }else{
        bit_rangediff_int2bit_rl(-rx[0], -rx[1], y, ny, b);
        nx = bit_rangediff_bit2int_rl(-rx[0], -rx[1], b, ret);
      }
    }else{
      if (revy){
        bit_rangediff_int2bit_lr(-rx[0], -rx[1], y, ny, b);
        nx = bit_rangediff_bit2int_lr_rev(-rx[0], -rx[1], b, ret);
      }else{
        bit_rangediff_int2bit_rl(rx[1], rx[0], y, ny, b);
        nx = bit_rangediff_bit2int_rl(rx[1], rx[0], b, ret);
      }
    }
  }else{
    if (revx){
      if (revy){
        bit_rangediff_int2bit_rl(rx[0], rx[1], y, ny, b);
        nx = bit_rangediff_bit2int_rl_rev(rx[0], rx[1], b, ret);
      }else{
        bit_rangediff_int2bit_lr(-rx[1], -rx[0], y, ny, b);
        nx = bit_rangediff_bit2int_lr(-rx[1], -rx[0], b, ret);
      }
    }else{
      if (revy){
        bit_rangediff_int2bit_rl(-rx[1], -rx[0], y, ny, b);
        nx = bit_rangediff_bit2int_rl_rev(-rx[1], -rx[0], b, ret);
      }else{
        bit_rangediff_int2bit_lr(rx[0], rx[1], y, ny, b);
        nx = bit_rangediff_bit2int_lr(rx[0], rx[1], b, ret);
      }
    }
  }
  ret_ = Rf_lengthgets(ret_, nx);  // no REPROTECT needed as long as we leave function immediately
  UNPROTECT(1);
  return(ret_);
}


SEXP R_bit_unique(SEXP b_, SEXP i_, SEXP range_, SEXP na_rm_){
  SEXP ret_;
  bitint *b = (bitint*) INTEGER(b_);
  int na_rm = asLogical(na_rm_);
  int *i = INTEGER(i_);
  int *r = INTEGER(range_);
  int n = LENGTH(i_);
  PROTECT( ret_ = allocVector(INTSXP,n) );
  int *ret = INTEGER(ret_);
  if (na_rm == NA_INTEGER){
    n = bit_unique_compareNA(i, n, r[0], b, ret);
  }else if (na_rm == FALSE){
    n = bit_unique_incomparableNA(i, n, r[0], b, ret);
  }else{  // na_rm == TRUE
    n = bit_unique_removeNA(i, n, r[0], b, ret);
  }
  ret_ = Rf_lengthgets(ret_, n);  // no REPROTECT needed as long as we leave function immediately
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_duplicated(SEXP b_, SEXP i_, SEXP range_, SEXP ret_, SEXP na_rm_){
  bitint *b = (bitint*) INTEGER(b_);
  bitint *ret = (bitint*) INTEGER(ret_);
  int na_rm = asLogical(na_rm_);
  int *i = INTEGER(i_);
  int *r = INTEGER(range_);
  int n = LENGTH(i_);
  if (na_rm == NA_INTEGER){
    bit_duplicated_compareNA(i, n, r[0], b, ret);
  }else if (na_rm == FALSE){
    bit_duplicated_incomparableNA(i, n, r[0], b, ret);
  }else{  // na_rm == TRUE
    bit_duplicated_removeNA(i, n, r[0], b, ret);
  }
  return(ret_);
}

SEXP R_bit_in_table(SEXP b_, SEXP i_, SEXP t_, SEXP range_na_, SEXP ret_){
  bitint *b = (bitint*) INTEGER(b_);
  bitint *ret = (bitint*) INTEGER(ret_);
  int *i = INTEGER(i_);
  int *t = INTEGER(t_);
  int ni = LENGTH(i_);
  int nt = LENGTH(t_);
  int *range_na = INTEGER(range_na_);
  if (range_na[2]>0)
    bit_in_table_NA(t, nt, i, ni, range_na[0], range_na[1], b, ret);
  else
    bit_in_table_NoNA(t, nt, i, ni, range_na[0], range_na[1], b, ret);
  return(ret_);
}

SEXP R_bit_table_in(SEXP b_, SEXP i_, SEXP t_, SEXP range_na_, SEXP ret_){
  bitint *b = (bitint*) INTEGER(b_);
  bitint *ret = (bitint*) INTEGER(ret_);
  int *i = INTEGER(i_);
  int *t = INTEGER(t_);
  int ni = LENGTH(i_);
  int nt = LENGTH(t_);
  int *range_na = INTEGER(range_na_);
  if (range_na[2]>0)
    bit_table_in_NA(t, nt, i, ni, range_na[0], range_na[1], b, ret);
  else
    bit_table_in_NoNA(t, nt, i, ni, range_na[0], range_na[1], b, ret);
  return(ret_);
}


SEXP R_bit_union(SEXP b_, SEXP x_, SEXP y_, SEXP range_na_){
  SEXP ret_;
  bitint *b = (bitint*) INTEGER(b_);
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  int *range_na = INTEGER(range_na_);
  int n=nx+ny;
  PROTECT( ret_ = allocVector(INTSXP,n) );
  int *ret = INTEGER(ret_);
  if (range_na[2]>0){
    n = bit_union_NA(x, nx, y, ny, range_na[0], b, ret);
  }else{
    n = bit_union_NoNA(x, nx, y, ny, range_na[0], b, ret);
  }
  ret_ = Rf_lengthgets(ret_, n);  // no REPROTECT needed as long as we leave function immediately
  UNPROTECT(1);
  return(ret_);
}


SEXP R_bit_intersect(SEXP b_, SEXP x_, SEXP y_, SEXP range_na_){
  SEXP ret_;
  bitint *b = (bitint*) INTEGER(b_);
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  int *range_na = INTEGER(range_na_);
  int n;
  if (nx>ny)
    n = nx;
  else
    n = ny;
  PROTECT( ret_ = allocVector(INTSXP,n) );
  int *ret = INTEGER(ret_);
  if (range_na[2]>0){
    n = bit_intersect_NA(x, nx, y, ny, range_na[0], range_na[1], b, ret);
  }else{
    n = bit_intersect_NoNA(x, nx, y, ny, range_na[0], range_na[1], b, ret);
  }
  ret_ = Rf_lengthgets(ret_, n);  // no REPROTECT needed as long as we leave function immediately
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_setdiff(SEXP b_, SEXP x_, SEXP y_, SEXP range_na_){
  SEXP ret_;
  bitint *b = (bitint*) INTEGER(b_);
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  int *range_na = INTEGER(range_na_);
  int n=nx;
  PROTECT( ret_ = allocVector(INTSXP,n) );
  int *ret = INTEGER(ret_);
  if (range_na[2]>0){
    n = bit_setdiff_NA(x, nx, y, ny, range_na[0], range_na[1], b, ret);
  }else{
    n = bit_setdiff_NoNA(x, nx, y, ny, range_na[0], range_na[1], b, ret);
  }
  ret_ = Rf_lengthgets(ret_, n);  // no REPROTECT needed as long as we leave function immediately
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_symdiff(SEXP bx_, SEXP by_, SEXP x_, SEXP y_, SEXP range_na_, SEXP xhasNA_, SEXP yhasNA_){
  SEXP ret_;
  bitint *bx = (bitint*) INTEGER(bx_);
  bitint *by = (bitint*) INTEGER(by_);
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  int *range_na = INTEGER(range_na_);
  int xhasNA = asLogical(xhasNA_);
  int yhasNA = asLogical(yhasNA_);
  int n=nx+ny;
  PROTECT( ret_ = allocVector(INTSXP,n) );
  int *ret = INTEGER(ret_);
  if (range_na[2]>0){
    n = bit_symdiff_NA(x, nx, y, ny, range_na[0], xhasNA, yhasNA, bx, by, ret);
  }else{
    n = bit_symdiff_NoNA(x, nx, y, ny, range_na[0], bx, by, ret);
  }
  ret_ = Rf_lengthgets(ret_, n);  // no REPROTECT needed as long as we leave function immediately
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_setequal(SEXP bx_, SEXP by_, SEXP x_, SEXP y_, SEXP range_na_){
  SEXP ret_;
  bitint *bx = (bitint*) INTEGER(bx_);
  bitint *by = (bitint*) INTEGER(by_);
  int *x = INTEGER(x_);
  int *y = INTEGER(y_);
  int nx = LENGTH(x_);
  int ny = LENGTH(y_);
  int *range_na = INTEGER(range_na_);
  PROTECT( ret_ = allocVector(LGLSXP,1) );
  if (range_na[2]>0){
    LOGICAL(ret_)[0] = bit_setequal_NA(x, nx, y, ny, range_na[0], range_na[1], bx, by);
  }else{
    LOGICAL(ret_)[0] = bit_setequal_NoNA(x, nx, y, ny, range_na[0], range_na[1], bx, by);
  }
  UNPROTECT(1);
  return(ret_);
}


SEXP R_bit_anyDuplicated(SEXP b_, SEXP i_, SEXP range_, SEXP na_rm_){
  bitint *b = (bitint*) INTEGER(b_);
  int na_rm = asLogical(na_rm_);
  int *i = INTEGER(i_);
  int *r = INTEGER(range_);
  int n = LENGTH(i_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  if (na_rm == NA_INTEGER){
    INTEGER(ret_)[0] = bit_anyDuplicated_compareNA(i, n, r[0], b);
  }else if (na_rm == FALSE){
    INTEGER(ret_)[0] = bit_anyDuplicated_incomparableNA(i, n, r[0], b);
  }else{  // na_rm == TRUE
    INTEGER(ret_)[0] = bit_anyDuplicated_removeNA(i, n, r[0], b);
  }
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_sumDuplicated(SEXP b_, SEXP i_, SEXP range_, SEXP na_rm_){
  bitint *b = (bitint*) INTEGER(b_);
  int na_rm = asLogical(na_rm_);
  int *i = INTEGER(i_);
  int *r = INTEGER(range_);
  int n = LENGTH(i_);
  int counter[2];
  bit_sumDuplicatedNA(i, n, r[0], b, counter);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  if (na_rm == NA_INTEGER){
    INTEGER(ret_)[0] = counter[0] + ((counter[1]>0) ? (counter[1] - 1) : 0);
  }else if (na_rm == FALSE){
    INTEGER(ret_)[0] = counter[0];
  }else{  // na_rm == TRUE
    INTEGER(ret_)[0] = counter[0] + counter[1];
  }
  UNPROTECT(1);
  return(ret_);
}


#define HANDLE_TRUE                     \
d = i - li;                             \
li = i;                                 \
if (d==ld){                             \
  ln++;                                 \
}else{                                  \
  val[c] = ld;                          \
  len[c] = ln;                          \
  s+=ln;                                \
  c++;                                  \
  if (c==n2){                           \
    R_Free(val);                          \
    R_Free(len);                          \
    last = NA_INTEGER; j=j1 + 1; break; \
  }                                     \
  ld = d;                               \
  ln = 1;                               \
}

/* last=0 means aborting rle */

SEXP R_bit_as_hi(SEXP b_, SEXP range_, SEXP offset_)
{
  bitint *b = (bitint*) INTEGER(b_);
  R_len_t b_length = LENGTH(b_);
  int *range = INTEGER(range_);
  int offset = asInteger(offset_);
  SEXP ret_, first_, dat_, last_, len_, retnames_, rlepackclass_;
  int protectcount = 0;
  
  register bitint word;
  register int k=(range[0]-1)%BITS;
  register int j=(range[0]-1)/BITS;
  int k1=(range[1]-1)%BITS;
  int j1=(range[1]-1)/BITS;
  int first = NA_INTEGER;
  int last = -1;  /* setting this to NA_INTEGER means: abort rle */

  int  c = 0;                   /* rle position */
  register int  i = NA_INTEGER; /* position     */
  register int li = NA_INTEGER; /* last position */
  register int  d = NA_INTEGER; /* difference */
  register int ld = NA_INTEGER; /* last difference */
  register int ln = 0;          /* counter of last difference */
  int s = 1;                    /* sum of TRUE */

  /* begin determine first and first increment d (stored in last_diff ld) */
  if (b_length > 0 && j < j1) {
    word = b[j];
    for(; k<BITS; k++){
      //Rprintf(" pre1 j=%d k=%d i=%d\n", j,k, j*BITS+k);
      if (word & mask1[k]){
        if (first==NA_INTEGER)
          first = j*BITS + k;
        else{
          li = j*BITS + k;
          ld = li - first;
          ln = 1;
          break;
        }
      }
    }
    if (!ln){
      for (j++; j<j1; j++){
        word = b[j];
        for(k=0; k<BITS; k++){
          //Rprintf("main1 j=%d k=%d i=%d\n", j,k, j*BITS+k);
          if (word & mask1[k]){
            if (first==NA_INTEGER)
              first = j*BITS + k;
            else{
              li = j*BITS + k;
              ld = li - first;
              ln = 1;
              j = j1 + 1; break;;
            }
          }
        }
      }
    }
    k=0;
  }
  if (b_length > 0 && !ln && j==j1 && k<=k1) {
    word = b[j];
    for(; k<=k1; k++){
      //Rprintf("post1 j=%d k=%d i=%d\n", j,k, j*BITS+k);
      if (word & mask1[k]){
        if (first==NA_INTEGER)
          first = j*BITS + k;
        else{
          li = j*BITS + k;
          ld = li - first;
          ln = 1;
          break;
        }
      }
    }
  }
  /* end determine first and first increment d */


  if (first!=NA_INTEGER) {  /* we have found at least one TRUE position */
    int n = range[1] - first;
  
    //Rprintf("CHECK: first=%d last=%d range[0]=%d range[1]=%d n=%d ln=%d\n", first, last, range[0], range[1], n, ln);
  
    if (ln && n >= 3) {
      /* see function intrle in package ff:
      max RAM requirement 2x, but rle only if at least 2/3 savings,
      using 2 instead of 3 would need 50% more time,
      have max RAM requirement 2.5x for savings of any size
      NOTE that n is a fuzzy worst case estimate of the number of TRUEs
      i.e. in some cases we miss the rle abort and use rle although simple positions would cost less RAM
      */
      int *val, *len;
      int n2 = n / 3;
      val = R_Calloc(n2, int);
      len = R_Calloc(n2, int);
      
      i=first+ld;
      k=(i+1)%BITS;
      j=(i+1)/BITS;
      
      //Rprintf("first=%d li=%d\n", first, li);
      
      
      /* begin determine increments */
      if (b_length > 0 && j < j1) {
        word = b[j];
        for(; k<BITS; k++) {
          i++;
          //Rprintf(" pre2 j=%d k=%d j*BITS+k=%d i=%d\n", j, k, j*BITS+k, i);
          if (word & mask1[k]){
            HANDLE_TRUE
          }
        }
        if (last!=NA_INTEGER) { /* not aborted rle */
          for (j++; j<j1; j++){
            word = b[j];
            for(k=0; k<BITS ;k++) {
              i++;
              //Rprintf("main2 j=%d k=%d j*BITS+k=%d i=%d\n", j, k, j*BITS+k, i);
              if (word & mask1[k]) {
                HANDLE_TRUE
              }
            }
          }
        }
        k = 0;
      }
      if (b_length > 0 && last != NA_INTEGER && j == j1) { /* not aborted rle */
        word = b[j];
        for(; k<=k1; k++) {
          i++;
          //Rprintf("post2 j=%d k=%d j*BITS+k=%d i=%d\n", j, k, j*BITS+k, i);
          if (word & mask1[k]){
            HANDLE_TRUE
          }
        }
      }
      if (last != NA_INTEGER) { /* not aborted rle */
        int *values, *lengths;
        SEXP lengths_, values_, datnames_, rleclass_;
        s += ln;
        val[c] = ld;
        len[c] = ln;
        c++;
        first++;
        last = range[1] - (i-li);
        /* end determine increments */
        PROTECT( values_ = allocVector(INTSXP, c) );
        values = INTEGER(values_);
        for (i=0;i<c;i++)
          values[i] = val[i];
        R_Free(val);
        PROTECT( lengths_ = allocVector(INTSXP, c) );
        lengths = INTEGER(lengths_);
        for (i=0;i<c;i++)
          lengths[i] = len[i];
        R_Free(len);
        
        PROTECT( dat_ = allocVector(VECSXP, 2) );
        PROTECT( datnames_ = allocVector(STRSXP, 2));
        PROTECT( rleclass_ = allocVector(STRSXP, 1));
        
        SET_STRING_ELT(datnames_, 0, mkChar("lengths"));
        SET_STRING_ELT(datnames_, 1, mkChar("values"));
        SET_STRING_ELT(rleclass_, 0, mkChar("rle"));
        SET_VECTOR_ELT(dat_, 0, lengths_);
        SET_VECTOR_ELT(dat_, 1, values_);
        setAttrib(dat_, R_NamesSymbol, datnames_);
        classgets(dat_, rleclass_);
        
        protectcount += 5;
      }
      
    } else {
      last = NA_INTEGER; /* abort rle */
    }

    /* if rle aborted, do the simple positions */
    
    if (last == NA_INTEGER) {
      int *dat;
      
      first++;
      s = bit_sum(b, first, range[1]);
      PROTECT( dat_ = allocVector(INTSXP, s) );
      dat = INTEGER(dat_);
      //Rprintf("1: offset=%d first=%d last=%d\n", offset, first, last);
      bit_which_positive(b, dat, first, range[1], offset);
      last = dat[s-1] - offset;
      //Rprintf("2: offset=%d first=%d last=%d\n", offset, first, last);
      
      protectcount++;
    }
  
  } else {
    /* all FALSE */
    last = NA_INTEGER;
    s = 0;
    PROTECT( dat_ = allocVector(INTSXP,0) );
    protectcount++;
  }

  PROTECT(first_ = allocVector(INTSXP, 1));
  PROTECT(last_ = allocVector(INTSXP, 1));
  PROTECT(len_ = allocVector(INTSXP, 1));
  //Rprintf("3: offset=%d first=%d last=%d\n", offset, first, last);
  INTEGER(first_)[0] = offset + first;
  INTEGER(last_)[0] = offset + last;
  INTEGER(len_)[0] = s;
  PROTECT(ret_ = allocVector(VECSXP, 4));

  PROTECT(retnames_ = allocVector(STRSXP, 4));
  SET_STRING_ELT(retnames_, 0, mkChar("first"));
  SET_STRING_ELT(retnames_, 1, mkChar("dat"));
  SET_STRING_ELT(retnames_, 2, mkChar("last"));
  SET_STRING_ELT(retnames_, 3, mkChar("len"));
  SET_VECTOR_ELT(ret_, 0, first_);
  SET_VECTOR_ELT(ret_, 1, dat_);
  SET_VECTOR_ELT(ret_, 2, last_);
  SET_VECTOR_ELT(ret_, 3, len_);
  setAttrib(ret_, R_NamesSymbol, retnames_);

  PROTECT(rlepackclass_ = allocVector(STRSXP, 1));
  SET_STRING_ELT(rlepackclass_, 0, mkChar("rlepack"));
  classgets(ret_, rlepackclass_);

  protectcount += 6;

  UNPROTECT(protectcount);
  return ret_;
}


#undef HANDLE_TRUE


// i_ must be either all pos or all neg
// we expect negative subscripts i_ to be sorted
// pos and neg may contain zeros and out of range
// but only pos may contain NAs
SEXP R_bit_extract(SEXP b_, SEXP i_){
  SEXP l_;
  bitint *b = (bitint*) INTEGER(b_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP bvirt = PROTECT(getAttrib(b_, virt));
  SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  int nb = asInteger(bleng);
  UNPROTECT(4);
  int *l, *i = INTEGER(i_);
  int nl, ni = LENGTH(i_);
  if (ni==0){
    nl = 0;
    PROTECT( l_ = allocVector(LGLSXP,nl) );
    // only neg is sorted, hence we must identify neg
    // upper end of neg may be zero, , hence we must query lower end
    // NA is also <0 but not allowed with neg, hence we must exclude NA
  }else if(i[0]<0 && i[0] != NA_INTEGER){ 
    PROTECT( l_ = allocVector(LGLSXP,nb) );
    l = LOGICAL(l_);
    nl = bit_extract_but_sorted(b, nb, i, ni, l); // nl < n if any effective i_
    if (nl<nb){
      l_ = Rf_lengthgets(l_, nl);  // no REPROTECT needed as long as we leave function immediately
    }
  }else{
    PROTECT( l_ = allocVector(LGLSXP,ni) );
    l = LOGICAL(l_);
    nl = bit_extract_unsorted(b, nb, i, ni, l); // nl < n if i_ contains zeros
    if (nl<ni){
      l_ = Rf_lengthgets(l_, nl);  // no REPROTECT needed as long as we leave function immediately
    }
  }
  UNPROTECT(1);
  return(l_);
}

// this alters b_
// we expect subscripts i_ to be sorted
// either all pos or all neg
// pos and neg may contain zeros
// only neg may contain out of range
// only pos may contain NAs
SEXP R_bit_replace(SEXP b_, SEXP i_, SEXP l_){
  bitint *b = (bitint*) INTEGER(b_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP bvirt = PROTECT(getAttrib(b_, virt));
  SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  int nb = asInteger(bleng);
  UNPROTECT(4);
  int *i = INTEGER(i_);
  int *l = LOGICAL(l_);
  int ni = LENGTH(i_);
  int nl = LENGTH(l_);
  if (ni>0){
    if(i[0]<0 && i[0] != NA_INTEGER){ 
      if (nl==1) bit_replace_but_sorted_one(b, nb, i, ni, l[0]);
      else if (nl>=(nb-ni)) bit_replace_but_sorted(b, nb, i, ni, l);
      else bit_replace_but_sorted_recycle(b, nb, i, ni, l, nl);
    }else{
      if (nl==1) bit_replace_unsorted_one(b, i, ni, l[0]);
      else if (nl>=ni) bit_replace_unsorted(b, i, ni, l);
      else bit_replace_unsorted_recycle(b, i, ni, l, nl);
    }
  }
  return(b_);
}


// this alters b_
SEXP R_bit_not(SEXP b_){
  bitint *b = (bitint*) INTEGER(b_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP bvirt = PROTECT(getAttrib(b_, virt));
  SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  int nb = asInteger(bleng);
  UNPROTECT(4);
  bit_not(b, nb);
  return(b_);
}

// this alters ret_
SEXP R_bit_and(SEXP b1_, SEXP b2_, SEXP ret_){
  bitint *b1 = (bitint*) INTEGER(b1_);
  bitint *b2 = (bitint*) INTEGER(b2_);
  bitint *ret = (bitint*) INTEGER(ret_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP bvirt = PROTECT(getAttrib(b1_, virt));
  SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  int nb = asInteger(bleng);
  UNPROTECT(4);
  bit_and(b1, b2, ret, nb);
  return(ret_);
}

// this alters ret_
SEXP R_bit_or(SEXP b1_, SEXP b2_, SEXP ret_){
  bitint *b1 = (bitint*) INTEGER(b1_);
  bitint *b2 = (bitint*) INTEGER(b2_);
  bitint *ret = (bitint*) INTEGER(ret_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP bvirt = PROTECT(getAttrib(b1_, virt));
  SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  int nb = asInteger(bleng);
  UNPROTECT(4);
  bit_or(b1, b2, ret, nb);
  return(ret_);
}

// this alters ret_
SEXP R_bit_xor(SEXP b1_, SEXP b2_, SEXP ret_){
  bitint *b1 = (bitint*) INTEGER(b1_);
  bitint *b2 = (bitint*) INTEGER(b2_);
  bitint *ret = (bitint*) INTEGER(ret_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP bvirt = PROTECT(getAttrib(b1_, virt));
  SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  int nb = asInteger(bleng);
  UNPROTECT(4);
  bit_xor(b1, b2, ret, nb);
  return(ret_);
}

// this alters ret_
SEXP R_bit_equal(SEXP b1_, SEXP b2_, SEXP ret_){
  bitint *b1 = (bitint*) INTEGER(b1_);
  bitint *b2 = (bitint*) INTEGER(b2_);
  bitint *ret = (bitint*) INTEGER(ret_);
  SEXP virt = PROTECT(install("virtual"));   
  SEXP leng = PROTECT(install("Length"));
  SEXP bvirt = PROTECT(getAttrib(b1_, virt));
  SEXP bleng = PROTECT(getAttrib(bvirt, leng));
  int nb = asInteger(bleng);
  UNPROTECT(4);
  bit_equal(b1, b2, ret, nb);
  return(ret_);
}


SEXP R_bit_sum(SEXP b_, SEXP range_){
  bitint *b = (bitint*) INTEGER(b_);
  int *range = INTEGER(range_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  INTEGER(ret_)[0] = bit_sum(b, range[0],  range[1]);
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_all(SEXP b_, SEXP range_){
  bitint *b = (bitint*) INTEGER(b_);
  int *range = INTEGER(range_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,1) );
  LOGICAL(ret_)[0] = bit_all(b, range[0],  range[1]);
  UNPROTECT(1);
  return(ret_);
}


SEXP R_bit_any(SEXP b_, SEXP range_){
  bitint *b = (bitint*) INTEGER(b_);
  int *range = INTEGER(range_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(LGLSXP,1) );
  LOGICAL(ret_)[0] = bit_any(b, range[0],  range[1]);
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_min(SEXP b_, SEXP range_){
  bitint *b = (bitint*) INTEGER(b_);
  int *range = INTEGER(range_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  INTEGER(ret_)[0] = bit_min(b, range[0],  range[1]);
  UNPROTECT(1);
  return(ret_);
}

SEXP R_bit_max(SEXP b_, SEXP range_){
  bitint *b = (bitint*) INTEGER(b_);
  int *range = INTEGER(range_);
  SEXP ret_;
  PROTECT( ret_ = allocVector(INTSXP,1) );
  INTEGER(ret_)[0] = bit_max(b, range[0],  range[1]);
  UNPROTECT(1);
  return(ret_);
}


/* performance tests without bit
static void filter_getset(int *l1, int *l2, int n){
  int i;
  for (i=0; i<n; i++){
    if (l1[i])
      l2[i] = 1;
    else
      l2[i] = 0;
  }
}

// this alters l2_
SEXP R_filter_getset(SEXP l1_, SEXP l2_){
  int *l1 = LOGICAL(l1_);
  int *l2 = LOGICAL(l2_);
  int n = LENGTH(l1_);
  filter_getset(l1, l2, n);
  return(l2_);
}
*/



/* some experiments - just ignore
static void bit_sample(bitint *b, int *x, int *y, int *z, int nb, int ns){
register int ib, j, jb, k, ii, nz;
// sampling phase
for (ii=0,nz=0; ii<nb; ii++){
ib = randIndex(nb);
j = ib/BITS;
k = ib%BITS;
if (!(b[j] & mask1[k])){
b[j] |= mask1[k];
z[nz++] = x[ib];
if (nz==ns)
return;
}
}
int nj= nb/BITS;
ii = 0;
for (j=0,jb=0; j<nj; j++,jb+=BITS){
for (k=0; k<BITS; k++){
if (!(b[j] & mask1[k])){
y[ii++] = x[jb + k];
}
}
}
nj= nb%BITS;
for (k=0; k<nj; k++){
if (!(b[j] & mask1[k])){
y[ii++] = x[jb + k];
}
}
nj= ii/BITS + (ii%BITS ? 1 : 0);
for (j=0;j<nj;j++)
b[j] = 0;
bit_sample(b, y, y, z+nz, ii, ns-nz);
}


SEXP R_bit_sample1(SEXP x_, SEXP b_, SEXP ns_){
bitint *b = (bitint*) INTEGER(b_);
int n = LENGTH(x_);
 // SEXP virt = PROTECT(install("virtual"));   
 // SEXP leng = PROTECT(install("Length"));
 // SEXP bvirt = PROTECT(getAttrib(b1_, virt));
 // SEXP bleng = PROTECT(getAttrib(bvirt, leng));
 // int nb = asInteger(bleng);
 // UNPROTECT(4);
 int ns = asInteger(ns_);
SEXP y_,z_;
PROTECT( y_ = allocVector(INTSXP,n) );
PROTECT( z_ = allocVector(INTSXP,ns) );
int *x = INTEGER(x_);
int *y = INTEGER(y_);
int *z = INTEGER(z_);
GetRNGstate();
bit_sample(b, x, y, z, n, ns);
PutRNGstate();
UNPROTECT(2);
return(z_);
}

SEXP R_bit_sample(SEXP x_, SEXP ns_, SEXP replace_){
int n = LENGTH(x_);
int ns = asInteger(ns_);
int replace = asLogical(replace_);
SEXP y_,z_;
PROTECT( z_ = allocVector(INTSXP,ns) );
int *x = INTEGER(x_);
int *z = INTEGER(z_);
int i,j,t;
GetRNGstate();
if (replace){
for (i=0;i<ns;i++){
j = randIndex(n);
z[i] = x[j];
}
}else{
PROTECT( y_ = allocVector(INTSXP,n) );
int *y = INTEGER(y_);
for (i=0;i<n;i++)
y[i] = x[i];
for (i=0;i<ns;i++){
j = i+randIndex(n-i);
z[i] = y[j];
y[j] = y[i];
}
UNPROTECT(1);
}
PutRNGstate();
UNPROTECT(1);
return(z_);
}
*/


#undef BITS 
#undef LASTBIT
#undef TRUE

