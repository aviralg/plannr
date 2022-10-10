
#' @export
print.plan <- function(x, ...) {
    print(plan_to_expr(x))
    invisible(x)
}

#' TODO
#' @export
#' @importFrom cli cli_code
plan_show <- function(plan) {
    cli_code(format(plan_to_expr(plan)))
    invisible(plan)
}

plan_to_expr <- function(plan) {
    type <- plan_type(plan)

    name <- plan_name(plan)
    desc <- plan_desc(plan)

    plan_type <- if (type == PLAN_TYPE_FUN)       quote(plan_new)
                 else if (type == PLAN_TYPE_PASS) quote(plan_pass)
                 else if (type == PLAN_TYPE_FAIL) quote(plan_fail)
                 else if (type == PLAN_TYPE_ANY)  quote(plan_any)
                 else if (type == PLAN_TYPE_ALL)  quote(plan_all)
                 else if (type == PLAN_TYPE_NOT)  quote(plan_not)
                 else if (type == PLAN_TYPE_IF)   quote(plan_cond)

    expr <- substitute(PLAN_TYPE(NAME, DESC),
                       list(PLAN_TYPE = plan_type, NAME = name, DESC = desc))

    if(type == PLAN_TYPE_FUN) {
        expr[[length(expr) + 1]] <- plan_fun(plan)
    }

    deps <- plan_deps(plan)
    for(d in deps) {
        name <- plan_name(d)
        expr[[length(expr) + 1]] <- as.name(name)
    }

    expr
}
