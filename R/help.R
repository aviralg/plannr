
par_dflt_str <- function(par) {
    dflt <- par_dflt(par)
    if(identical(dflt, MISSING)) {
        ""
    }
    else {
        toString(dflt)
    }
}

#' @importFrom stringr str_pad
pad_max <- function(chrs) {
    str_pad(chrs, max(nchar(chrs)))
}

#' @importFrom purrr map_chr
#' @importFrom tibble tibble
par_tbl <- function(pars) {

    names <- map_chr(pars, par_name)
    dflts <- map_chr(pars, par_dflt_str)
    descs <- map_chr(pars, par_desc)

    tibble(name = pad_max(names),
           dflt = pad_max(dflts),
           desc = pad_max(descs))
}

#' TODO
#' @export
#' @importFrom cli cli_h1 cli_text cli_rule style_italic
#' @importFrom cli col_green cli_li cli_ul cli_end
#' @importFrom purrr pwalk
plan_help <- function(plan, recurse = FALSE, indent = 0) {

    name <- plan_name(plan)
    deps <- map_chr(plan_deps(plan), plan_name)

    heading <- if(length(deps) != 0) {
                   "{name}: {.pkg {deps}}"
               }
               else {
                   "{name}"
               }

    cli_h1(heading)
    cat("  ", style_italic(col_green(plan_desc(plan))), "\n")

    pars <- list()
    if(length(pars) != 0) {
        tbl <- par_tbl(pars)

       pwalk(tbl, function(name, dflt, desc) {
           cat("   ")
           cli_text("{.strong {name}}  {.code {dflt}}  {.emph {desc}}")
        })
    }

    cli_rule()

    invisible(plan)
}
