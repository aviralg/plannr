
data_make <- function(dir) {
    obj <- new.env(parent = emptyenv())

    obj$dir  <- dir

    dir_create(dir, recurse = TRUE)

    class(obj) <- "plan_data"

    lockEnvironment(obj, bindings = TRUE)

    obj
}

#' @export
#' @importFrom fs dir_info
print.plan_data <- function(x, ...) {
    ## TODO: print plan name and data dir
    info <- dir_info(data_dir(x))
    info[, c("path",
             "type",
             "size",
             "permissions",
             "modification_time",
             "user",
             "group",
             "access_time",
             "change_time",
             "birth_time")]

}

#' Query plan's data directory
#'
#' @param data Data object obtained from plan using `plan_data`
#'
#' @export
data_dir <- function(data) {
    data$dir
}

#' Construct path to a file or directory relative to plan's data directory
#'
#' @param ... character vectors, if any values are NA, the result will also be
#'   NA. The paths follow the recycling rules used in the tibble package,
#'   namely that only length 1 arguments are recycled.
#' @param ext An optional extension to append to the generated path.
#'
#' @export
#'
#' @importFrom fs path
data_file <- function(data, ..., ext = "") {
    path(data_dir(data), ..., ext)
}

#' Read and write data
#'
#' @description
#' These functions first construct a file path relative to plan's data directory
#' and read or write content.
#'
#' * `data_read()` reads data
#' * `data_write()` writes data
#'
#' @param data Data object obtained from plan using `plan_data`
#' @param ... character vectors, if any values are NA, the result will also be
#'   NA. The paths follow the recycling rules used in the tibble package,
#'   namely that only length 1 arguments are recycled.
#' @param ext An optional extension to append to the generated path.
#' @param content Content to write to file
#'
#' @export
#' @name read
data_read <- function(data, ..., ext = "") {
    ext_read(data_file(data, ..., ext))
}

#' @export
#' @rdname read
data_write <- function(data, ..., ext = "", content) {
    ext_write(data_file(data, ..., ext), content)
    content
}
