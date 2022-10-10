make_opt_args <- function() {
    opt_args <- new.env(parent = emptyenv())

    opt_args$file <- NA_character_
    opt_args$args <- FALSE
    opt_args$plan <- NA_character_
    opt_args$main_args <- new.env(parent = emptyenv())
    opt_args$plan_args <- new.env(parent = emptyenv())

    opt_args
}

plan_find <- function(plan, name) {

    if(plan_name(plan) == name) {
        return(plan)
    }

    deps <- plan_deps(plan)

    for(d in deps) {
        res = plan_find(d, name)

        if(!is.null(res)) {
            return(res)
        }
    }

    NULL
}

#' @importFrom purrr map_chr
plan_pars <- function(plan, recurse = FALSE) {
    pars <- character(0)

    if(plan_type(plan) == PLAN_TYPE_FUN) {
        fun <- plan_fun(plan)
        pars <- names(formals(fun))
    }

    dep_pars <- character(0)

    if(recurse) {
        deps <- plan_deps(plan)
        for(dep in deps) {
            dep_pars <- c(dep_pars, plan_pars(dep, recurse))
        }
    }

    unique(c(pars, dep_pars))
}

process_cmd <- function(plan, opt_args) {

    main_args <- opt_args$main_args
    help  <- get0("help", main_args, ifnotfound = FALSE)
    store <- get0("store", main_args, ifnotfound = NA_character_)
    force <- get0("force", main_args, ifnotfound = FALSE)

    plan_name <- opt_args$plan

    if(help) {
        plan_help(plan, TRUE)
        return(NULL)
    }

    sub_plan <- plan_find(plan, plan_name)

    if(is.null(sub_plan)) {
        msg <- sprintf("invalid plan name: %s", plan_name)
        stop(msg)
    }

    expected_pars <- plan_pars(sub_plan, recurse = TRUE)

    pars <- as.list.environment(opt_args$plan_args,
                                all.names = TRUE,
                                sorted = TRUE)

    extras <- setdiff(names(pars), expected_pars)

    if(length(extras) > 0) {
        msg <- sprintf("unused parameters: %s",
                       paste(extras, collapse = "\n"))
        warning(msg)
    }

    plan_run(sub_plan, pars, force, store)
}


main_arg_type <- list(
    help  = list(arg_type = NULL),
    store = list(arg_type = "character"),
    force = list(arg_type = NULL)
)

add_main_arg <- function(env, name, arg) {
    spec <- main_arg_type[[name]]

    if(is.null(spec)) {
        msg <- sprintf("invalid parameter --%s", name)
        stop(msg)
    }

    if(exists(name, env)) {
        stop(sprintf("repeated occurrence of parameter '%s'", name))
    }

    if (is.null(spec$arg_type)) {
        if(length(arg) > 1) {
            msg <- sprintf("parameter '%s' does not require an argument", name)
            stop(msg)
        }
        val <- TRUE
    }
    else if(length(arg) == 1) {
        msg <- sprintf("parameter '%s' requires an argument", name)
        stop(msg)
    }
    else if(spec$arg_type == "character") {
        val <- arg[2]
    }
    else {
        msg <- sprintf("unhandled parameter '%s' with argument type '%s'", spec$name, spec$arg_type)
        stop(msg)
    }

    env[[name]] <- val
}

#' TODO
#' @export
plan_cmd <- function(plan,
                     args = commandArgs(),
                     cnvt = function(name, arg) {
                         if(missing(arg)) TRUE else eval(parse(text = arg))
                     }) {
    opt_args <- make_opt_args()

    tryCatch({
        for(arg in args) {
            update_opt_args(arg, opt_args, cnvt)
        }
    },
    error = function(e) {
        plan_help(plan, recurse = TRUE)
        stop(e)
    })

    process_cmd(plan, opt_args)
}

#' @importFrom fs path_file
#' @importFrom stringr str_split str_trim str_starts str_sub
update_opt_args <- function(arg, opt_args, cnvt) {

    arg <- str_split(arg, fixed("="), n = 2)[[1]]

    n <- length(arg)

    name <- arg[1]

    is_arg <- str_starts(name, "--")


    ## do this while --file is not encountered
    if(is.na(opt_args$file)) {
        if(name == "--file") {
            opt_args$file <- path_file(arg[2])
        }
    }
    ## do this if --file has been encountered but not --args
    else if(!opt_args$args) {
        if(name == "--args") {
            opt_args$args <- TRUE
        }
    }
    ## if not an arg, then probably a plan name
    else if(!is_arg) {
        if(n > 1) {
            msg <- sprintf("invalid option specification: '%s'. \n",
                           paste(arg, collapse = "="))
            stop(msg)
        }

        if(!is.na(opt_args$plan)) {
            msg <- sprintf("cannot specify multiple subcommands (%s, %s) simultaneously. \n",
                           opt_args$plan,
                           arg[1])
            stop(msg)
        }

        opt_args$plan <- arg[1]

    }
    ## this is a global arg, either global or plan
    else if(is.na(opt_args$plan)) {
        name <- str_trim(str_sub(name, 3))
        add_main_arg(opt_args$main_args, name, arg)
    }
    else {
        name <- str_trim(str_sub(name, 3))
        val  <- if(n > 1) cnvt(name, arg[2]) else cnvt(name)
        env  <- opt_args$plan_args

        if(exists(name, env)) {
            stop(sprintf("repeated occurrence of parameter '%s'", name))
        }

        env[[name]] <- val
    }

    NULL
}
