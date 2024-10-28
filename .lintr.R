linters = all_linters(
  packages = "lintr",
  infix_spaces_linter(exclude_operators=c('EQ_SUB', 'EQ_FORMALS')),
  line_length_linter(100L),
  # TODO(org/repo#000) or TODO(#000) allowed as canonical TODO comments.
  todo_comment_linter(except_regex = "TODO\\((?:[a-zA-Z0-9-]+/[a-zA-Z0-9._-]+)?#[0-9]+\\)"),
  commented_code_linter = NULL,
  condition_call_linter = NULL,
  # TODO(#34): Activate with limit of 25.
  cyclocomp_linter = NULL,
  function_argument_linter = NULL,
  nonportable_path_linter = NULL,
  object_name_linter = NULL,
  quotes_linter = NULL,
  # TODO(#19): Activate this; it's not trivial to replace on some old R versions.
  rep_len_linter = NULL,
  unreachable_code_linter = NULL,
  # TODO(#2441): Activate this when it supports = for assignments
  assignment_linter = NULL,
  # TODO(r-lib/lintr#2172): Exclude the below from vignettes/, enforce elsewhere
  # Finer-tuned handling for some of these.
  undesirable_function_linter(modify_defaults(
    defaults = default_undesirable_functions,
    library = NULL,
    options = NULL,
    par = NULL,
    sapply = NULL
  )),
  # Will be allow_scoped=TRUE
  implicit_assignment_linter = NULL,
  implicit_integer_linter = NULL,
  library_call_linter = NULL,
  sample_int_linter = NULL
)

exclusions = list("tests/testthat.R")
