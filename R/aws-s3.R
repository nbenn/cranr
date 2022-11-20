
get_keys <- function(...) {
  chr_ply(aws.s3::get_bucket(...), `[[`, "Key")
}

get_md5 <- function(keys = NULL, ...) {

  objects <- aws.s3::get_bucket(...)

  md5 <- chr_ply(objects, `[[`, "ETag")
  md5 <- gsub("\"", "", md5)

  if (is.null(keys)) {
    return(md5)
  }

  md5[match(keys, chr_ply(objects, `[[`, "Key"))]
}

rm_keys <- function(keys, ...) {

  if (length(keys)) {
    message("removing keys\n  -", paste0(keys, collapse = "\n  -"))
  }

  res <- lgl_ply(keys, aws.s3::delete_object, ...)

  if (any(!res)) {
    warning("The following keys were not deleted\n  -",
            paste0(keys[!res], collapse = "\n  -"))
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
  res <- lgl_ply(res, `[[`, 1L)

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

  full_paths <- file.path(dir, paths)

  dirs <- file.info(full_paths)[["isdir"]]
  files <- full_paths[!dirs]

  final_paths <- lapply(full_paths[dirs], list.files, recursive = TRUE)
  final_paths <- Map(file.path, paths[dirs], final_paths)
  final_paths <- file.path(dir, unlist(final_paths))

  stopifnot(all(file.exists(files)))

  files <- c(files, final_paths)

  keys <- sub(paste0("^", dir, "/?"), "", files)

  rm_extra_keys(keys, ...)
  rm_old_keys(files, keys, ...)

  upload_new_files(files, keys, ...)

  invisible(NULL)
}

#' @rdname upload_repo
#' @export
download_repo <- function(dir = ".", ...) {

  ensure_empty_dir(dir)

  aws.s3::s3sync(dir, direction = "download", verbose = FALSE, ...)

  invisible(NULL)
}

ensure_empty_dir <- function(dir) {

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  if (length(list.files(dir))) {
    stop("Cannot use a non-empty directory as target")
  }

  invisible(NULL)
}
