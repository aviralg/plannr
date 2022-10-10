.ext_db <- new.env(parent = emptyenv())

#' @importFrom readr write_lines
int_wtr <- function(...) {
    function(content, file) {
        write_lines(content, file, ...)
    }
}

#' @importFrom readr read_lines
int_rdr <-  function(...) {
    function(file) {
        as.integer(read_lines(file, ...))
    }
}

#' @importFrom readr write_lines
dbl_wtr <-  function(...) {
    function(content, file) {
        write_lines(content, file, ...)
    }
}

#' @importFrom readr read_lines
dbl_rdr <-  function(...) {
    function(file) {
        as.double(read_lines(file, ...))
    }
}

#' @importFrom readr write_lines
lgl_wtr <-  function(...) {
    function(content, file) {
        write_lines(content, file, ...)
    }
}

#' @importFrom readr read_lines
lgl_rdr <-  function(...) {
    function(file) {
        as.logical(read_lines(file, ...))
    }
}

#' @importFrom readr write_lines
chr_wtr <-  function(...) {
    function(content, file) {
        write_lines(content, file, ...)
    }
}

#' @importFrom readr read_lines
chr_rdr <-  function(...) {
    function(file) {
        as.character(read_lines(file, ...))
    }
}

#' @importFrom readr write_lines
cpx_wtr <-  function(...) {
    function(content, file) {
        write_lines(content, file, ...)
    }
}

#' @importFrom readr read_lines
cpx_rdr <-  function(...) {
    function(file) {
        as.complex(read_lines(file, ...))
    }
}

#' @importFrom readr write_file
raw_wtr <-  function(...) {
    function(content, file) {
        write_file(content, file, ...)
    }
}

#' @importFrom readr read_file_raw
raw_rdr <-  function(...) {
    function(file) {
        read_file_raw(file, ...)
    }
}

rds_wtr <-  function(...) {
    function(content, file) {
        base::saveRDS(object = content, file = file, ...)
    }
}

rds_rdr <-  function(...) {
    function(file) {
        base::readRDS(file, ...)
    }
}

#' @importFrom fst write_fst
fst_wtr <-  function(...) {
    function(content, file) {
        fst::write_fst(content, file, ...)
    }
}

#' @importFrom fst read_fst
fst_rdr <-  function(...) {
    function(file) {
        fst::read_fst(file, ...)
    }
}

#' @importFrom readr write_csv
csv_wtr <-  function(...) {
    function(content, file) {
        readr::write_csv(content, file, ...)
    }
}

#' @importFrom readr read_csv cols
csv_rdr <-  function(..., col_types = cols()) {
    function(file) {
        readr::read_csv(file, ..., col_types = col_types)
    }
}

#' @importFrom readr write_file
txt_wtr <-  function(...) {
    function(content, file) {
        readr::write_file(content, file, ...)
    }
}

#' @importFrom readr read_file
txt_rdr <-  function(...) {
    function(file) {
        read_file(file, ...)
    }
}

#' TODO
#' @export
ext_set <- function(ext, rdr, wtr) {
    res <- .ext_db[[ext]]

    if (is.null(res)) {
        res <- new.env(hash = TRUE, parent = emptyenv())
        assign(ext, res, envir = .ext_db)
    }

    res$rdr <- rdr
    res$wtr <- wtr

    invisible(NULL)
}

#' TODO
#' @export
ext_rdr <- function(ext) {
    res <- .ext_db[[ext]]

    if (is.null(res)) {
        stop(sprintf("no reader set for extension '%s'", ext))
    }

    res$rdr
}

#' TODO
#' @export
ext_wtr <- function(ext) {
    res <- .ext_db[[ext]]

    if (is.null(res)) {
        stop(sprintf("no writer set for extension '%s'", ext))
    }

    res$wtr
}

#' TODO
#' @export
#' @importFrom fs path_ext
ext_read <- function(path) {
    ext <- path_ext(path)
    reader <- ext_rdr(ext)
    reader(path)
}

#' TODO
#' @export
#' @importFrom fs path_ext
ext_write <- function(path, content) {
    ext <- path_ext(path)
    writer <- ext_wtr(ext)
    writer(content, path)
}


ext_init <- function() {
    ext_set("int", int_rdr(), int_wtr())
    ext_set("dbl", dbl_rdr(), dbl_wtr())
    ext_set("lgl", lgl_rdr(), lgl_wtr())
    ext_set("chr", chr_rdr(), chr_wtr())
    ext_set("cpx", cpx_rdr(), cpx_wtr())
    ext_set("raw", raw_rdr(), raw_wtr())
    ext_set("rds", rds_rdr(), rds_wtr())
    ext_set("fst", fst_rdr(), fst_wtr())
    ext_set("csv", csv_rdr(), csv_wtr())
    ext_set("txt", txt_rdr(), txt_wtr())
}

ext_init()
