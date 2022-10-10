

plan_run_helper <- function(plan, pars, flags) {

    state <- make_state(plan, flags)

    if(!is.na(flags$store)) {
        data <- data_make(state$data_dir)
        plan_data_set(plan, data)
    }

    type <- plan_type(plan)

    res <-
        if     (type == PLAN_TYPE_FUN)  { plan_fun_run(plan, pars, flags, state)        }
        else if(type == PLAN_TYPE_PASS) { plan_pass_run(plan, pars, flags, state)       }
        else if(type == PLAN_TYPE_FAIL) { plan_fail_run(plan, pars, flags, state)       }
        else if(type == PLAN_TYPE_ANY)  { plan_any_run(plan, pars, flags, state)        }
        else if(type == PLAN_TYPE_ALL)  { plan_all_run(plan, pars, flags, state)        }
        else if(type == PLAN_TYPE_NOT)  { plan_not_run(plan, pars, flags, state)        }
        else if(type == PLAN_TYPE_IF)   { plan_if_run(plan, pars, flags, state)       }
        else                            { stop(sprintf("unhandled plan type %d", type)) }

    plan_res_set(plan, res)

    if(!is.na(flags$store)) {
        res_write(res, state$res_dir)
    }

    res
}

make_state <- function(plan, flags) {

    store <- flags$store

    if(is.na(store)) {
        return(new.env(parent = emptyenv()))
    }

    ## TODO: exec_dir should be made into a object
    plan_dir <- path(store, plan_name(plan))
    exec_dir <- path(plan_dir, "exec")
    res_dir  <- path(plan_dir, "res")
    data_dir <- path(plan_dir, "data")
    sout_file <- path(res_dir, "sout.txt")

    list2env(
        list(plan_dir = plan_dir,
             exec_dir = exec_dir,
             res_dir = res_dir,
             data_dir = data_dir),
        parent = emptyenv()
    )
}

update_state <- function(env, ...) {
    args <- list(...)
    keys <- names(args)

    for(i in 1:length(args)) {
        key <- keys[[i]]
        val <- args[[i]]
        env[[key]] <- val
    }
}

time_to_df <- function(time) {
    time <- unname(unclass(time))
    data.frame(process = time[1], real = time[2])
}

#' @importFrom withr with_output_sink with_tempdir
#' @importFrom rlang new_environment
#' @importFrom bench bench_time
#' @importFrom tibble tibble
#' @importFrom withr with_dir
#' @importFrom fs dir_create
#' @importFrom utils capture.output
plan_fun_run <- function(plan, pars, flags, state) {

    if(!flags$force) {
        deps <- plan_deps(plan)
        for(d in deps) {
            res <- plan_run_helper(d, pars, flags)
            if(!res_succ(res)) {
                return(res)
            }
        }
    }

    fun <- plan_fun(plan)

    par_names <- names(formals(fun))

    pars$self <- plan

    args <- pars[par_names]

    exec_dir <- if(!is.na(flags$store)) {
                    dir_create(state$exec_dir, recurse = TRUE)
                    state$exec_dir
                }
                else {
                    getwd()
                }

    sout <- capture.output(
        time <- with_dir(exec_dir,
                         bench_time({
                             result <- tryCatch(
                                 list(do.call(fun, args), 0),
                                 error = function(e) {
                                     ## by printing, this error gets recorded on stdout
                                     ## which is redirected to a file during evaluation
                                     print(e)
                                     list(NULL, 1)
                                 })
                         })),
        type = c("output", "message"))

    code <- result[[2]]

    res_make(sout, code, time_to_df(time))
}

plan_pass_run <- function(plan, pars, flags, state) {

    deps <- plan_deps(plan)
    dep <- deps[[1]]

    time <- bench_time({
        res <- plan_run_helper(dep, pars, flags)
    })

    code <- 0

    sout <- res_sout(res)

    res_make(sout, code, time_to_df(time))
}

plan_fail_run <- function(plan, pars, flags, state) {

    deps <- plan_deps(plan)
    dep <- deps[[1]]

    time <- bench_time({
        res <- plan_run_helper(dep, pars, flags)
    })

    code <- 1

    sout <- res_sout(res)

    res_make(sout, code, time_to_df(time))
}

plan_any_run <- function(plan, pars, flags, state) {

    deps <- plan_deps(plan)

    res_list <- list()

    code <- 0

    time <- bench_time({
        for (dep in deps) {
            res <- plan_run_helper(dep, pars, flags)
            res_list <- c(res_list, res)
            code <- res_code(res)
            if (res_succ(res)) {
                break
            }
        }
    })

    sout_vec <- map_chr(res_list, res_sout)
    sout <- paste(sout_vec, collapse = "\n")

    res_make(sout, code, time_to_df(time))
}

plan_all_run <- function(plan, pars, flags, state) {

    deps <- plan_deps(plan)

    res_list <- list()

    ## assume that we will have a successful run
    code <- 0

    time <- bench_time({
        for(dep in deps) {
            res <- plan_run_helper(dep, pars, flags)
            res_list <- c(res_list, res)
            if(!res_succ(res)) {
                code <- res_code(res)
                break
            }
        }
    })

    sout_vec <- map_chr(res_list, res_sout)
    sout <- paste(sout_vec, collapse = "\n")

    res_make(sout, code, time_to_df(time))
}

plan_not_run <- function(plan, pars, flags, state) {

    deps <- plan_deps(plan)
    dep <- deps[[1]]

    time <- bench_time({
        res <- plan_run_helper(dep, pars, flags)
    })

    code <- if(res_code(res) == 0) 1 else 0

    sout <- res_sout(res)

    res_make(sout, code, time_to_df(time))
}

plan_if_run <- function(plan, pars, flags, state) {

    deps <- plan_deps(plan)
    cplan <- deps[[1]]
    tplan <- deps[[2]]
    fplan <- deps[[3]]

    time <- bench_time({
        cres <- plan_run_helper(cplan, pars, flags)
        res <- if(res_code(cres) == 0) {
                   plan_run_helper(tplan, pars, flags)
               } else {
                   plan_run_helper(fplan, pars, flags)
               }
    })

    code <- res_code(res)
    sout <- res_sout(res)

    res_make(sout, code, time_to_df(time))
}

#' TODO
#' @export
#' @importFrom fs path
#' @importFrom withr with_dir
plan_run <- function(plan,
                     pars,
                     force = FALSE,
                     store = path(getwd(), "plan")) {
    print(force)
    print(store)

    flags <- list(force = force, store = store)
    print(flags)

    ## make sure that wd is reset even if error happens
    with_dir(getwd(),
             plan_run_helper(plan, pars, flags))
}
