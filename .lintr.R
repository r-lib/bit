linters = all_linters(
  packages = "lintr",
  semicolon_linter(allow_compound = TRUE),
  # TODO(r-lib/lintr#2172): Finer-tuned handling for some of these.
  undesirable_function_linter(modify_defaults(
    defaults = default_undesirable_functions,
    library = NULL,
    options = NULL,
    par = NULL,
    sapply = NULL
  )),
  assignment_linter = NULL,
  brace_linter = NULL,
  commas_linter = NULL,
  commented_code_linter = NULL,
  condition_call_linter = NULL,
  cyclocomp_linter = NULL,
  function_argument_linter = NULL,
  function_left_parentheses_linter = NULL,
  # TODO(r-lib/lintr#2172): Exclude this from vignettes/ enforce elsewhere with allow_scoped=TRUE
  implicit_assignment_linter = NULL,
  # TODO(r-lib/lintr#2172): Exclude this from vignettes/ enforce elsewhere.
  implicit_integer_linter = NULL,
  indentation_linter = NULL,
  infix_spaces_linter = NULL,
  library_call_linter = NULL,
  line_length_linter = NULL,
  nonportable_path_linter = NULL,
  object_name_linter = NULL,
  paren_body_linter = NULL,
  quotes_linter = NULL,
  # TODO(#19): Activate this; it's not trivial to replace on some old R versions.
  rep_len_linter = NULL,
  # TODO(r-lib/lintr#2172): Exclude this from vignettes/ enforce elsewhere.
  sample_int_linter = NULL,
  spaces_inside_linter = NULL,
  spaces_left_parentheses_linter = NULL,
  todo_comment_linter = NULL,
  unreachable_code_linter = NULL
)
