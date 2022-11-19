#' Initialize and populate a drat repository
#'
#' By creating the `src/contrib` path and a top-level `index.html` file,
#' `init_repo()` initializes a drat repository. Following that, packages can
#' be added using `insert_pkg()`/`insert_pkgs()`.
#'
#' @param dir Repository directory
#' @param ... Passed on to other methods (`update_repo()` in case of
#'   `init_repo()` and [tools::write_PACKAGES] for `insert_pkg()`)
#'
#' @importFrom utils contrib.url untar unzip
#' @export
init_repo <- function(dir = ".", ...) {

  ensure_empty_dir(dir)

  writeLines("## Drat Repo", file.path(dir, "README.md"))

  dir.create(file.path(dir, "src", "contrib"), recursive = TRUE)

  writeLines(
    "<!doctype html><title>empty placeholder file</title>",
    file.path(dir, "index.html")
  )

  update_repo(dir, ...)

  invisible(NULL)
}

#' @param check_cran Check CRAN for bin paths
#' @param base_url CRAN URL
#'
#' @rdname init_repo
#' @export
update_repo <- function(dir = ".", check_cran = FALSE,
                        base_url = get_cran_url()) {

  platform <- c("windows", "macosx",
                get_flavors("macosx", base_url, check_cran))

  ctb_paths <- lapply(platform, get_versions, base_url, check_cran)
  ctb_paths <- unlist(ctb_paths, recursive = FALSE, use.names = FALSE)

  for (path in ctb_paths) {
    add_path(file.path(dir, path))
  }

  existing <- list.dirs(file.path(dir, "bin"), full.names = FALSE,
                        recursive = TRUE)
  existing <- file.path("bin", existing)
  existing <- sub("^\\./", "", existing)[grepl("contrib/", existing)]

  to_rm <- setdiff(existing, sub("/$", "", ctb_paths))

  if (length(to_rm)) {
    unlink(to_rm, recursive = TRUE)
  }

  invisible(NULL)
}

add_path <- function(path) {

  pp <- file.path(path, "PACKAGES")
  ppz <- paste0(pp, ".gz")

  dir.create(path, recursive = TRUE)

  if (file.exists(pp) && file.exists(ppz)) {
    return(invisible(FALSE))
  }

  if (!file.exists(pp)) {
    writeLines(character(0), pp)
  }

  if (!file.exists(ppz)) {
    write_lines_gz(character(0), ppz)
  }

  invisible(TRUE)
}

write_lines_gz <- function(text, filename, ...) {
  con <- gzfile(filename)
  on.exit(close(con))
  writeLines(text, con, ...)
}

#' @param pkg,pkgs R package bundle(s)
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

get_links_or_null <- function(url) {

  if (!pkgs_available("httr2", "xml2")) {
    return(NULL)
  }

  req <- httr2::request(url)
  req <- httr2::req_error(req, is_error = function(resp) FALSE)
  res <- httr2::req_perform(req)

  if (httr2::resp_is_error(res)) {
    return(NULL)
  }

  xml <- xml2::read_html(
    httr2::resp_body_string(res, encoding = "UTF-8"),
    encoding = "UTF-8"
  )

  vapply(xml2::xml_find_all(xml, "//a"), xml2::xml_attr, character(1L), "href")
}

pkgs_available <- function(...) {
  all(vapply(c(...), requireNamespace, logical(1L), quietly = TRUE))
}

get_cran_url <- function(fallback = "https://cloud.r-project.org") {

  res <- unname(getOption("repos")["CRAN"])

  if (is.na(res) || !length(res) || identical(res, "")) {
    res <- fallback
  }

  res
}

get_flavors <- function(os, base_url, check_cran = FALSE) {

  if (check_cran) {
    res <- get_links_or_null(paste(base_url, "bin", os, sep = "/"))
  } else {
    res <- NULL
  }

  if (is.null(res)) {
    res <- c("mavericks", "el-capitan", "big-sur-arm64")
  } else {
    res <- res[grepl("/$", res)]
    res <- res[!grepl("^http|^/|tools|base|old|contrib", res)]
  }

  paste(os, sub("/$", "", res), sep = "/")
}

get_versions <- function(flavor, base_url, check_cran = FALSE) {

  if (check_cran) {
    res <- get_links_or_null(
      paste(base_url, "bin", flavor, "contrib", sep = "/")
    )
  } else {
    res <- NULL
  }

  if (is.null(res)) {
    res <- c("3.4", "3.5", "3.6", "4.0", "4.1", "4.2", "4.3")
  } else {
    res <- res[grepl("/$", res)]
    res <- res[!grepl("^/|^r-", res)]
  }

  file.path("bin", flavor, "contrib", res)
}
