
res_make <- function(sout, code, time, dir = NULL) {
    obj <- new.env(parent = emptyenv())

    obj$sout <- sout
    obj$code <- code
    obj$time <- time
    obj$dir  <- dir

    class(obj) <- "plan_res"

    ## TODO: need to unlock in res_dir
    ## lockEnvironment(obj, bindings = TRUE)

    obj
}

res_read <- function(dir) {
    sout <- ext_read(path(dir, "sout.txt"))
    code <- ext_read(path(dir, "code.int"))
    time <- ext_read(path(dir, "time.csv"))
    res_make(sout, code, time, dir)
}

res_write <- function(res, dir) {
    res$dir <- dir
    dir_create(dir, recurse = TRUE)
    ext_write(path(dir, "sout.txt"), res_sout(res))
    ext_write(path(dir, "code.int"), res_code(res))
    ext_write(path(dir, "time.csv"), res_time(res))
}

#' TODO
#' @export
res_dir <- function(res) {
    dir <- res$dir
    if(is.null(dir)) {
        stop("no directory associated with plan result")
    }
    dir
}

#' TODO
#' @export
res_sout <- function(res) {
    res$sout
}

#' TODO
#' @export
res_code <- function(res) {
    res$code
}

#' TODO
#' @export
res_time <- function(res) {
    res$time
}

#' TODO
#' @export
res_succ <- function(res) {
    res_code(res) == 0
}

