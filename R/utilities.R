INVALID_FILENAME_CHARACTERS <- c("/", "\\", "?", "%", "*", ":", "|", "\"", "<", ">", ".", ",", ";", "=", " ")

#' @importFrom stringr str_detect fixed
#' @importFrom purrr map_lgl
validate_name <- function(name) {
    if (any(map_lgl(INVALID_FILENAME_CHARACTERS, function(c) str_detect(name, fixed(c))))) {
        msg <- sprintf("name '%s' should not have the following characters: ",
                       name,
                       paste(INVALID_FILENAME_CHARACTERS, collapse = " "))
        stop(msg)
    }
}

expr_to_chr <- function(val, collapse = "\n") {
    if(missing(val)) ""
    else paste(deparse(val), collapse = collapse)
}


is_subset <- function(small, large) {
    setequal(intersect(small, large), small)
}
