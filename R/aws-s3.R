
get_meta <- function(field, keys = NULL, ...) {

  res <- aws.s3::get_bucket(...)

  if (!is.null(keys)) {
    res <- res[match(keys, chr_ply(res, `[[`, "Key"))]
  }

  if (is.null(field) || isTRUE(is.na(field))) {
    return(res)
  }

  chr_ply(res, `[[`, field)
}

get_keys <- function(...) get_meta("Key", ...)

get_md5 <- function(...) gsub("\"", "", get_meta("ETag", ...))

get_mtime <- function(...) {
  strptime(get_meta("LastModified", ...), format = "%FT%H:%M:%OSZ", tz = "UTC")
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

  stopifnot(length(files) == length(keys))

  common <- intersect(get_keys(...), keys)
  cksums <- get_md5(common, ...)

  if (any(grep("-", cksums))) {
    warning("One or more keys are multi-part objects without checksums. ",
            "These are always assumed to be different.")
  }

  different <- common[cksums != tools::md5sum(files[match(common, keys)])]

  if (length(different)) {
    file_mtime <- file.info(files[match(different, keys)])[["mtime"]]
    to_rm <- different[get_mtime(different, ...) <= file_mtime]
  }

  rm_keys(to_rm, ...)
}

add_new_files <- function(files, keys = files, ...) {

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
#'   methods of [aws.s3::s3HTTP()]
#'
#' @export
upload_repo <- function(dir = "." , paths = c("index.html", "bin", "src"),
                        ...) {

  stopifnot(requireNamespace("aws.s3", quietly = TRUE))

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

  add_new_files(files, keys, ...)

  invisible(NULL)
}

#' @rdname upload_repo
#' @export
download_repo <- function(dir = ".", ...) {

  stopifnot(requireNamespace("aws.s3", quietly = TRUE))

  ensure_empty_dir(dir)

  keys <- get_keys(NULL, ...)

  res <- Map(aws.s3::save_object, keys, file = file.path(dir, keys),
             MoreArgs = list(...))
  res <- lgl_ply(res, `[[`, 1L)

  if (any(!res)) {
    warning("The following keys were not downloaded\n  - ",
            paste0(keys[res], collapse = "\n  - "))
  }

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
