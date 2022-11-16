drat_version_regex <- "[0-9]\\.[0-9]$"
drat_contrib_version_regex <- paste0("contrib/", drat_version_regex)

#' Initialize and populate a drat repository
#'
#' By creating the `src/contrib` path and a top-level `index.html` file,
#' `init_repo()` initializes a drat repository. Following that, packages can
#' be added using `insert_pkg()`/`insert_pkgs()`.
#'
#' @param dir Repository directory
#'
#' @author Dirk Eddelbuettel and Nicolas Bennett
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
#' @author Dirk Eddelbuettel and Nicolas Bennett
#' @export
insert_pkg <- function(pkg, dir = ".", ...) {

  stopifnot(file.exists(file), dir.exists(dir),
            file.exists(file.path(dir, "index.html")))

  pkginfo <- get_pkg_info(file)
  pkgtype <- get_pkg_type(file, pkginfo)

  pkgdir <- normalizePath(
    contrib_url(dir, pkgtype, pkginfo["Rmajor"]),
    mustWork = FALSE
  )

  if (!(file.exists(pkgdir) || dir.create(pkgdir, recursive = TRUE))) {
    stop("Directory ", pkgdir, " couldn't be created\n")
  }

  if (!file.copy(file, pkgdir, overwrite = TRUE)) {
    stop("File ", file, " can not be copied to ", pkgdir)
  }

  write_packages(dir, pkgtype, ...)

  invisible(NULL)
}

#' @rdname init_repo
#' @export
insert_pkgs <- function(pkgs, ...){
  lapply(pkgs, insert_pkg, ...)
  invisible(NULL)
}

#' @author Dirk Eddelbuettel and Nicolas Bennett
write_packages <- function(dir, type, ..., latestOnly = FALSE) {

  split_pkgtype <- strsplit(type,"\\.")[[1L]]

  pkgtype <- paste(
    split_pkgtype[seq.int(1L, min(2L, length(split_pkgtype)))],
    collapse = "."
  )

  if (pkgtype == "binary" && grepl("darwin", R.version$os)) {
    pkgtype <- "mac.binary"
  }

  tools::write_PACKAGES(dir, ..., type = pkgtype, latestOnly = latestOnly)
}

#' @author Dirk Eddelbuettel and Nicolas Bennett
get_pkg_info <- function(file) {

  stopifnot(file.exists(file))

  td <- tempdir()

  if (grepl(".zip$", file)) {
    # Windows
    unzip(file, exdir = td)
  } else if (grepl(".tgz$", file)) {
    # macOS
    untar(file, exdir = td)
  } else {
    # Source
    return(c("Source" = TRUE, "Rmajor" = NA, "osxFolder" = ""))
  }

  pkgname <- gsub("^([a-zA-Z0-9.]*)_.*", "\\1", basename(file))
  path <- file.path(td, pkgname, "DESCRIPTION")

  stopifnot(file.exists(path))

  builtstring <- read.dcf(path, 'Built')
  unlink(file.path(td, pkgname), recursive = TRUE)

  fields <- strsplit(builtstring, "; ")[[1]]
  names(fields) <- c("Rversion", "OSflavour", "Date", "OS")

  rmajor <- gsub("^R (\\d\\.\\d)\\.\\d.*", "\\1", fields["Rversion"])

  osxFolder <- switch(
    fields["OSflavour"],
    `x86_64-apple-darwin13.4.0` = "mavericks",
    `x86_64-apple-darwin15.6.0` = "el-capitan",
    `aarch64-apple-darwin20`    = "big-sur-arm64",
    ""
  )

  c(fields, "Rmajor" = unname(rmajor), "osxFolder" = osxFolder)
}

#' @author Jan Schulz, Dirk Eddelbuettel and Nicolas Bennett
get_pkg_type <- function(file, pkginfo = get_pkg_info(file)) {

  if (grepl("_.*\\.tar\\..*$", file)) {

    "source"

  } else if (grepl("_.*\\.tgz$", file)) {

    res <- "mac.binary"

    if (pkginfo["osxFolder"] == "") {

      if (package_version(pkginfo["Rmajor"]) < package_version("4.1")) {

        switch(
          pkginfo["Rmajor"],
          `3.2` = , `3.3` = paste0(res, ".mavericks"),
          `3.4` = , `3.5` = , `3.6` = paste0(res, ".el-capitan"),
          res
        )

      } else if (grepl("aarch64", pkginfo["OSflavour"])) {

        # ARM Mac for R >= 4.1
        "binary"
      }

    } else {

      stopifnot(pkginfo["osxFolder"] %in% c("mavericks", "el-capitan",
                                            "big-sur-arm64"))

      paste0(res, ".", pkginfo["osxFolder"])
    }

  } else if (grepl("_.*\\.zip$", file)) {

    "win.binary"

  } else {

    stop("Unknown package type", call. = FALSE)
  }
}

#' @author Dirk Eddelbuettel and Nicolas Bennett
type_to_url <- function(type, repos, version) {

  contrib_url <- contrib.url(repos = repos, type = type)

  if (is.null(version)) {

    return(contrib_url)

  } else if (is.na(version)) {

    if (type != "source") {

      extra_ctb <- list.dirs(
        gsub(drat_contrib_version_regex, "contrib", contrib_url),
        recursive = FALSE
      )

      contrib_url <- unique(c(contrib_url, extra_ctb))
    }

  } else {

    version <- package_version(version)

    contrib_url <- gsub(
      drat_contrib_version_regex,
      file.path("contrib", paste0(version$major,".",version$minor)),
      contrib_url
    )

  }

  contrib_url
}

#' @author Dirk Eddelbuettel and Nicolas Bennett
contrib_url <- function(repos, types = getOption("pkgType"), version = NULL) {

  urls <- lapply(types, type_to_url, repos, version)

  urls <- unlist(urls)
  names(urls) <- unlist(Map(rep, types, lengths(urls)))

  urls
}
