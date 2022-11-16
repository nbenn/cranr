
get_keys <- function(...) {
  vapply(aws.s3::get_bucket(...), `[[`, character(1L), "Key")
}

get_md5 <- function(keys = NULL, ...) {

  objects <- aws.s3::get_bucket(...)

  md5 <- vapply(objects, `[[`, character(1L), "ETag")
  md5 <- gsub("\"", "", md5)

  if (is.null(keys)) {
    return(md5)
  }

  md5[match(keys, vapply(objects, `[[`, character(1L), "Key"))]
}

rm_keys <- function(keys, ...) {

  if (length(keys)) {
    message("removing keys\n  -", paste0(keys, collapse = "\n  -"))
  }

  res <- vapply(keys, aws.s3::delete_object, logical(1L), ...)

  if (any(!res)) {
    warning("The following keys were not deleted\n  -",
            paste0(keys[res], collapse = "\n  -"))
  }

  invisible(NULL)
}

rm_extra_keys <- function(keys, ...) {
  rm_keys(setdiff(get_keys(...), keys), ...)
}

rm_old_keys <- function(files, keys, ...) {

  common <- intersect(get_keys(...), keys)

  to_rm <- get_md5(common, ...) != tools::md5sum(files[match(common, keys)])
  rm_keys(common[to_rm], ...)
}

upload_new_files <- function(files, keys = files, ...) {

  to_do <- setdiff(keys, get_keys(...))
  to_do <- match(to_do, keys)

  keys <- keys[to_do]

  if (length(keys)) {
    message("adding keys\n  - ", paste0(keys, collapse = "\n  - "))
  }

  res <- Map(aws.s3::put_object, files[to_do], keys, MoreArgs = list(...))
  res <- vapply(res, `[[`, logical(1L), 1L)

  if (any(!res)) {
    warning("The following keys were not uploaded\n  - ",
            paste0(keys[res], collapse = "\n  - "))
  }

  invisible(NULL)
}

#' Upload repo to s3 bucket
#'
#' Upload a CRAN-like repository for packages to an AWS S3 bucket.
#'
#' @param dir Directory of repo
#' @param paths Paths (relative to `dir`) to include in upload
#' @param ... Further arguments (such as `bucket`, `region`, etc.) passed to
#'   methods of [aws.s3]
#'
#' @export
upload_repo <- function(dir = "." , paths = c("index.html", "bin", "src"),
                        ...) {

  dirs <- list.dirs(dir, full.names = FALSE, recursive = FALSE)
  files <- setdiff(paths, dirs)

  files <- file.path(dir, files)

  stopifnot(all(file.exists(files)))

  dirs <- intersect(paths, dirs)

  for (path in dirs) {
    files <- c(files,
      list.files(file.path(dir, path), full.names = TRUE, recursive = TRUE)
    )
  }

  keys <- sub(paste0("^", dir, "/?"), "", files)

  rm_extra_keys(keys, ...)
  rm_old_keys(files, keys, ...)

  upload_new_files(files, keys, ...)

  invisible(NULL)
}

#' @rdname upload_repo
#' @export
download_repo <- function(dir = ".", ...) {

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  if (length(list.files(dir))) {
    stop("Cannot use a non-empty directory as target")
  }

  aws.s3::s3sync(dir, direction = "download", verbose = FALSE, ...)

  invisible(NULL)
}
