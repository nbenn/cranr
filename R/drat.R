#' Initialize and populate a drat repository
#'
#' By creating the `src/contrib` path and a top-level `index.html` file,
#' `init_repo()` initializes a drat repository. Following that, packages can
#' be added using `insert_pkg()`/`insert_pkgs()`.
#'
#' @param dir Repository directory
#'
#' @importFrom utils contrib.url untar unzip
#' @export
init_repo <- function(dir = ".") {

  ensure_empty_dir(dir)

  writeLines("## Drat Repo", file.path(dir, "README.md"))

  dir.create(file.path(dir, "src", "contrib"), recursive = TRUE)

  writeLines(
    "<!doctype html><title>empty placeholder file</title>",
    file.path(dir, "index.html")
  )

  invisible(NULL)
}

#' @param pkg,pkgs R package bundle(s)
#' @param ... Passed on to [tools::write_PACKAGES]
#'
#' @rdname init_repo
#' @export
insert_pkg <- function(pkg, dir = ".", ...) {

  stopifnot(file.exists(pkg), dir.exists(dir),
            file.exists(file.path(dir, "index.html")))

  if (grepl(".zip$", pkg) || grepl(".tgz$", pkg)) {
    stop("Currently only source packages are supported.")
  }

  pkgtype <- "source"

  pkgdir <- normalizePath(
    contrib.url(dir, type = pkgtype),
    mustWork = FALSE
  )

  if (!(file.exists(pkgdir) || dir.create(pkgdir, recursive = TRUE))) {
    stop("Directory ", pkgdir, " couldn't be created\n")
  }

  if (!file.copy(pkg, pkgdir, overwrite = TRUE)) {
    stop("File ", pkg, " can not be copied to ", pkgdir)
  }

  write_packages(pkgdir, pkgtype, ...)

  invisible(NULL)
}

#' @rdname init_repo
#' @export
insert_pkgs <- function(pkgs, ...){
  lapply(pkgs, insert_pkg, ...)
  invisible(NULL)
}

write_packages <- function(dir, type, ..., latestOnly = FALSE) {
  tools::write_PACKAGES(dir, ..., type = type, latestOnly = latestOnly)
}
