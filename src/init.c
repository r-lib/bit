#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP R_first_zero(SEXP);
extern SEXP R_int_is_asc_break(SEXP);
extern SEXP R_int_is_asc_none(SEXP);
extern SEXP R_int_is_asc_skip(SEXP);
extern SEXP R_int_is_desc_break(SEXP);
extern SEXP R_int_is_desc_none(SEXP);
extern SEXP R_int_is_desc_skip(SEXP);
extern SEXP R_int_rle(SEXP);
extern SEXP R_bit_all(SEXP, SEXP);
extern SEXP R_bit_and(SEXP, SEXP, SEXP);
extern SEXP R_bit_any(SEXP, SEXP);
extern SEXP R_bit_anyDuplicated(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_as_hi(SEXP, SEXP, SEXP);
extern SEXP R_bit_done();
extern SEXP R_bit_duplicated(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_equal(SEXP, SEXP, SEXP);
extern SEXP R_bit_extract(SEXP, SEXP);
extern SEXP R_bit_get_integer(SEXP, SEXP, SEXP);
extern SEXP R_bit_get_logical(SEXP, SEXP, SEXP);
extern SEXP R_bit_init(SEXP);
extern SEXP R_bit_in_table(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_intersect(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_max(SEXP, SEXP);
extern SEXP R_bit_min(SEXP, SEXP);
extern SEXP R_bit_not(SEXP);
extern SEXP R_bit_or(SEXP, SEXP, SEXP);
extern SEXP R_bit_rangediff(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_recycle(SEXP, SEXP);
extern SEXP R_bit_replace(SEXP, SEXP, SEXP);
extern SEXP R_bit_reverse(SEXP, SEXP);
extern SEXP R_bit_set_attr(SEXP, SEXP, SEXP);
extern SEXP R_bit_setdiff(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_setequal(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_set_integer(SEXP, SEXP, SEXP);
extern SEXP R_bit_set_logical(SEXP, SEXP, SEXP);
extern SEXP R_bit_shiftcopy(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_sort(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_sort_unique(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_sum(SEXP, SEXP);
extern SEXP R_bit_sumDuplicated(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_symdiff(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_table_in(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_union(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_unique(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bit_vecseq(SEXP, SEXP);
extern SEXP R_bit_which(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_bitwhich_representation(SEXP);
extern SEXP R_bit_xor(SEXP, SEXP, SEXP);
extern SEXP R_copy(SEXP, SEXP);
extern SEXP R_duplicate(SEXP);
extern SEXP R_firstNA(SEXP);
extern SEXP R_get_length(SEXP);
extern SEXP R_int_countsort(SEXP, SEXP, SEXP);
extern SEXP R_int_quicksort2(SEXP, SEXP, SEXP);
extern SEXP R_int_quicksort3(SEXP, SEXP, SEXP);
extern SEXP R_merge_anyDuplicated(SEXP, SEXP);
extern SEXP R_merge_duplicated(SEXP, SEXP);
extern SEXP R_merge_first(SEXP, SEXP);
extern SEXP R_merge_firstin(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_firstnotin(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_in(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_intersect(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_last(SEXP, SEXP);
extern SEXP R_merge_lastin(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_lastnotin(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_match(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_notin(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_rangediff(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_rangein(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_rangenotin(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_rangesect(SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_rev(SEXP);
extern SEXP R_merge_setdiff(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_setequal(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_sumDuplicated(SEXP, SEXP);
extern SEXP R_merge_symdiff(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_union(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP R_merge_unique(SEXP, SEXP);
extern SEXP R_still_identical(SEXP, SEXP);
extern SEXP R_range_na(SEXP);
extern SEXP R_range_nanozero(SEXP);
extern SEXP R_range_sortna(SEXP, SEXP, SEXP);
extern SEXP R_reverse(SEXP);
//extern SEXP R_get_refcnt(SEXP);
//extern SEXP R_set_refcnt(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"R_first_zero",              (DL_FUNC) &R_first_zero,              1},
    {"R_int_is_asc_break",        (DL_FUNC) &R_int_is_asc_break,        1},
    {"R_int_is_asc_none",         (DL_FUNC) &R_int_is_asc_none,         1},
    {"R_int_is_asc_skip",         (DL_FUNC) &R_int_is_asc_skip,         1},
    {"R_int_is_desc_break",       (DL_FUNC) &R_int_is_desc_break,         1},
    {"R_int_is_desc_none",        (DL_FUNC) &R_int_is_desc_none,          1},
    {"R_int_is_desc_skip",        (DL_FUNC) &R_int_is_desc_skip,          1},
    {"R_int_rle",                 (DL_FUNC) &R_int_rle,                   1},
    {"R_bit_all",                 (DL_FUNC) &R_bit_all,                 2},
    {"R_bit_and",                 (DL_FUNC) &R_bit_and,                 3},
    {"R_bit_any",                 (DL_FUNC) &R_bit_any,                 2},
    {"R_bit_anyDuplicated",       (DL_FUNC) &R_bit_anyDuplicated,       4},
    {"R_bit_as_hi",               (DL_FUNC) &R_bit_as_hi,               3},
    {"R_bit_done",                (DL_FUNC) &R_bit_done,                0},
    {"R_bit_duplicated",          (DL_FUNC) &R_bit_duplicated,          5},
    {"R_bit_equal",               (DL_FUNC) &R_bit_equal,               3},
    {"R_bit_extract",             (DL_FUNC) &R_bit_extract,             2},
    {"R_bit_get_integer",         (DL_FUNC) &R_bit_get_integer,         3},
    {"R_bit_get_logical",         (DL_FUNC) &R_bit_get_logical,         3},
    {"R_bit_init",                (DL_FUNC) &R_bit_init,                1},
    {"R_bit_in_table",            (DL_FUNC) &R_bit_in_table,            5},
    {"R_bit_intersect",           (DL_FUNC) &R_bit_intersect,           4},
    {"R_bit_max",                 (DL_FUNC) &R_bit_max,                 2},
    {"R_bit_min",                 (DL_FUNC) &R_bit_min,                 2},
    {"R_bit_not",                 (DL_FUNC) &R_bit_not,                 1},
    {"R_bit_or",                  (DL_FUNC) &R_bit_or,                  3},
    {"R_bit_rangediff",           (DL_FUNC) &R_bit_rangediff,           5},
    {"R_bit_recycle",             (DL_FUNC) &R_bit_recycle,             2},
    {"R_bit_replace",             (DL_FUNC) &R_bit_replace,             3},
    {"R_bit_reverse",             (DL_FUNC) &R_bit_reverse,             2},
    {"R_bit_set_attr",            (DL_FUNC) &R_bit_set_attr,            3},
    {"R_bit_setdiff",             (DL_FUNC) &R_bit_setdiff,             4},
    {"R_bit_setequal",            (DL_FUNC) &R_bit_setequal,            5},
    {"R_bit_set_integer",         (DL_FUNC) &R_bit_set_integer,         3},
    {"R_bit_set_logical",         (DL_FUNC) &R_bit_set_logical,         3},
    {"R_bit_shiftcopy",           (DL_FUNC) &R_bit_shiftcopy,           4},
    {"R_bit_sort",                (DL_FUNC) &R_bit_sort,                5},
    {"R_bit_sort_unique",         (DL_FUNC) &R_bit_sort_unique,         5},
    {"R_bit_sum",                 (DL_FUNC) &R_bit_sum,                 2},
    {"R_bit_sumDuplicated",       (DL_FUNC) &R_bit_sumDuplicated,       4},
    {"R_bit_symdiff",             (DL_FUNC) &R_bit_symdiff,             7},
    {"R_bit_table_in",            (DL_FUNC) &R_bit_table_in,            5},
    {"R_bit_union",               (DL_FUNC) &R_bit_union,               4},
    {"R_bit_unique",              (DL_FUNC) &R_bit_unique,              4},
    {"R_bit_vecseq",              (DL_FUNC) &R_bit_vecseq,              2},
    {"R_bit_which",               (DL_FUNC) &R_bit_which,               4},
    {"R_bitwhich_representation", (DL_FUNC) &R_bitwhich_representation, 1},
    {"R_bit_xor",                 (DL_FUNC) &R_bit_xor,                 3},
    {"R_copy",                    (DL_FUNC) &R_copy,                    2},
    {"R_duplicate",               (DL_FUNC) &R_duplicate,               1},
    {"R_firstNA",                 (DL_FUNC) &R_firstNA,                 1},
    {"R_get_length",              (DL_FUNC) &R_get_length,              1},
    {"R_int_countsort",           (DL_FUNC) &R_int_countsort,           3},
    {"R_int_quicksort2",          (DL_FUNC) &R_int_quicksort2,          3},
    {"R_int_quicksort3",          (DL_FUNC) &R_int_quicksort3,          3},
    {"R_merge_anyDuplicated",     (DL_FUNC) &R_merge_anyDuplicated,     2},
    {"R_merge_duplicated",        (DL_FUNC) &R_merge_duplicated,        2},
    {"R_merge_first",             (DL_FUNC) &R_merge_first,             2},
    {"R_merge_firstin",           (DL_FUNC) &R_merge_firstin,           4},
    {"R_merge_firstnotin",        (DL_FUNC) &R_merge_firstnotin,        4},
    {"R_merge_in",                (DL_FUNC) &R_merge_in,                4},
    {"R_merge_intersect",         (DL_FUNC) &R_merge_intersect,         5},
    {"R_merge_last",              (DL_FUNC) &R_merge_last,              2},
    {"R_merge_lastin",            (DL_FUNC) &R_merge_lastin,            4},
    {"R_merge_lastnotin",         (DL_FUNC) &R_merge_lastnotin,         4},
    {"R_merge_match",             (DL_FUNC) &R_merge_match,             5},
    {"R_merge_notin",             (DL_FUNC) &R_merge_notin,             4},
    {"R_merge_rangediff",         (DL_FUNC) &R_merge_rangediff,         4},
    {"R_merge_rangein",           (DL_FUNC) &R_merge_rangein,           4},
    {"R_merge_rangenotin",        (DL_FUNC) &R_merge_rangenotin,        4},
    {"R_merge_rangesect",         (DL_FUNC) &R_merge_rangesect,         4},
    {"R_merge_rev",               (DL_FUNC) &R_merge_rev,               1},
    {"R_merge_setdiff",           (DL_FUNC) &R_merge_setdiff,           5},
    {"R_merge_setequal",          (DL_FUNC) &R_merge_setequal,          5},
    {"R_merge_sumDuplicated",     (DL_FUNC) &R_merge_sumDuplicated,     2},
    {"R_merge_symdiff",           (DL_FUNC) &R_merge_symdiff,           5},
    {"R_merge_union",             (DL_FUNC) &R_merge_union,             5},
    {"R_merge_unique",            (DL_FUNC) &R_merge_unique,            2},
    {"R_still_identical",         (DL_FUNC) &R_still_identical,         2},
    {"R_range_na",                (DL_FUNC) &R_range_na,                1},
    {"R_range_nanozero",          (DL_FUNC) &R_range_nanozero,          1},
    {"R_range_sortna",            (DL_FUNC) &R_range_sortna,            3},
    {"R_reverse",                 (DL_FUNC) &R_reverse,                 1},
    //{"R_get_refcnt",              (DL_FUNC) &R_get_refcnt,              1},
    //{"R_set_refcnt",              (DL_FUNC) &R_set_refcnt,              2},
    {NULL, NULL, 0}
};

/* write.table(names(getDLLRegisteredRoutines("ff", addNames = TRUE)[[2]]), file="clipboard", sep="\t") */
void R_init_bit(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
