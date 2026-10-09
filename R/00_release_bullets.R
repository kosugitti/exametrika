#' Extra bullets for usethis::use_release_issue()
#'
#' usethis picks this up from the package namespace when it builds the
#' release checklist issue. The WORDLIST item exists because the list once
#' went unmaintained: 300+ stale findings buried two real typos in the
#' user-facing help for over a year (found 2026-08-19).
#'
#' @noRd
release_bullets <- function() {
  return(c(
    "Run `source(\"tools/spell_check.R\")`, register new legitimate terms in `inst/WORDLIST`, fix any real typos",
    "Run `tools/build_pkg.R`: it refuses to build unless `git status --porcelain` is empty, so commit first and never build from a dirty tree"
  ))
}
