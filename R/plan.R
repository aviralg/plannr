PLAN_TYPE_FUN  <- 1
PLAN_TYPE_PASS <- 3
PLAN_TYPE_FAIL <- 4
PLAN_TYPE_ANY  <- 5
PLAN_TYPE_ALL  <- 6
PLAN_TYPE_NOT  <- 7
PLAN_TYPE_IF <- 8

.state <- new.env(parent = emptyenv())
.state$id <- -1

get_id <- function() {
    .state$id <- .state$id + 1
    .state$id
}

plan_base <- function(type, name, desc, deps, fun = NULL) {

    validate_name(name)

    names(deps) <- map_chr(deps, plan_name)

    obj <- new.env(parent = emptyenv())

    obj$id   <- get_id()
    obj$type <- type
    obj$name <- name
    obj$desc <- desc
    obj$deps <- deps
    obj$fun  <- fun
    obj$data <- NULL
    obj$res  <- NULL


    class(obj) <- "plan"

    obj
}

#' Make plan objects
#'
#' * `plan_new()` constructs a plan object from function
#' * `plan_pass()` always succeeds regardless of the result of its dependency
#' * `plan_fail()` always fails regardless of the result of its dependency
#' * `plan_any()` succeeds if any of its dependencies succeed
#' * `plan_all()` succeeds if all of its dependencies succeed
#' * `plan_not()` reverses the result of its dependency
#' * `plan_if()`
#'
#' @param name Plan name.
#' @param desc Plan description.
#' @param fun  Plan function.
#' @param ...  Plan dependencies.
#' @param plan Dependency plan
#' @param cplan Dependency plan
#' @param tplan Dependency plan
#' @param fplan Dependency plan
#' @return A new plan.
#' @examples
#' plan_new("slicer", "Slice a df", function(self, df, len = 10) df[1:len, ])
#' @export
#' @name plan
plan_new <- function(name, desc, fun, ...) {
    stopifnot(is.function(fun))
    plan_base(PLAN_TYPE_FUN, name, desc, list(...), fun)
}


#' Basic plan combinators
#'
#' @description
#' These plan combinators can be used to forcibly fail or pass a plan object.
#'
#' * `plan_pass()` constructs a plan that succeeds even if `plan` fails
#' * `plan_fail()` constructs a plan that fails even if `plan` succeeds
#'
#' @param name Plan name.
#' @param desc Plan description.
#' @param plan Plan being wrapped
#'
#' @export
#' @rdname basicplan
plan_pass <- function(name, desc, plan) {
    plan_base(PLAN_TYPE_PASS, name, desc, list(plan))
}

#' @export
#' @rdname basicplan
plan_fail <- function(name, desc, plan) {
    plan_base(PLAN_TYPE_FAIL, name, desc, list(plan))
}


#' Logical plan combinators
#'
#' @description
#' These plan combinators are the equivalent of `|`, `&`, and `!` logical
#' operators for plan objects.
#' * `plan_any()` constructs a plan that succeeds if any of the plans succeed.
#' * `plan_all()` constructs a plan that succeeds only if all the plans succeed.
#' * `plan_not()` constructs a plan that negates a plan's result.
#'
#' @param name Plan name.
#' @param desc Plan description.
#' @param ...  One or more plan objects
#' @param plan Plan whose result will be negated
#'
#' @export
#' @rdname logicplan
plan_any <- function(name, desc, ...) {
    plan_base(PLAN_TYPE_ANY, name, desc, list(...))
}

#' @export
#' @rdname logicplan
plan_all <- function(name, desc, ...) {
    plan_base(PLAN_TYPE_ALL, name, desc, list(...))
}

#' @export
#' @rdname logicplan
plan_not <- function(name, desc, plan) {
    plan_base(PLAN_TYPE_NOT, name, desc, list(plan))
}

#' Conditional plan combinator
#'
#' @description
#' `plan_if()` constructs a control-flow plan that executes different plans
#' depending on a conditional plan.
#'
#' @param name Plan name.
#' @param desc Plan description.
#' @param cplan Plan that alters control-flow.
#' @param tplan Plan that is executed if `cplan` succeeds.
#' @param fplan Plan that is executed if `cplan` fails.
#'
#' @export
plan_if <- function(name, desc, cplan, tplan, fplan) {
    plan_base(PLAN_TYPE_IF, name, desc, list(cplan, tplan, fplan))
}

plan_id <- function(plan) {
    plan$id
}

plan_type <- function(plan) {
    plan$type
}


#' Query plan objects
#'
#' @description
#' * `plan_name()` returns the plan name.
#' * `plan_desc()` returns the plan description.
#' * `plan_deps()` returns a named list of plan's dependencies.
#' * `plan_dep()` returns plan's dependency with the given name.
#' * `plan_fun()` returns the plan's function.
#'
#' @details
#'
#' @export
#' @rdname queryplan
plan_name <- function(plan) {
    plan$name
}

#' @export
#' @rdname queryplan
plan_desc <- function(plan) {
    plan$desc
}

#' @export
#' @rdname queryplan
plan_deps <- function(plan) {
    plan$deps
}

#' @export
#' @rdname queryplan
plan_dep <- function(plan, name) {
    deps <- plan_deps(plan)
    deps[[name]]
}

#' @export
#' @rdname queryplan
plan_fun <- function(plan) {
    plan$fun
}
#' TODO
#' @export
plan_data <- function(plan) {
    plan$data
}

plan_data_set <- function(plan, data) {
    plan$data <- data
}

#' TODO
#' @export
plan_res <- function(plan) {
    plan$res
}

plan_res_set <- function(plan, res) {
    plan$res <- res
}
