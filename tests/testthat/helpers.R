rand_string <- function(length, alphabet = letters) {
  paste(sample(alphabet, length, TRUE), collapse = "")
}

rand_base_64 <- function(...) {
  sub("\\n", "", jsonlite::base64_enc(rand_string(...)))
}

s3_object <- function(file, dir = ".", bucket = "local",
                      storage_class = "STANDARD") {

  info <- file.info(file.path(dir, file))

  res <- list(
    Key = file,
    LastModified = format(info[["mtime"]], "%FT%H:%M:%S.000Z", tz = "UTC"),
    ETag = paste0("\"", tools::md5sum(file.path(dir, file)), "\""),
    Size = info[["size"]],
    Owner = digest::digest(info[["uname"]], "sha256"),
    StorageClass = storage_class,
    Bucket = bucket
  )

  structure(res, class = "s3_object")
}

s3_bucket <- function(dir, bucket = "local", region = "us-east-2",
                      prefix = list(), max_keys = 1000L, truncated = FALSE,
                      ...) {

  res <- Map(s3_object, list.files(dir, recursive = TRUE), dir, bucket, ...)
  names(res) <- rep("Contents", length(res))

  structure(
    res,
    `x-amz-id-2` = rand_base_64(56L),
    `x-amz-request-id` = rand_string(16L, c(LETTERS, 0:9)),
    date = format(Sys.time(), "%a, %d %b %Y %H:%M:%S %Z", tz = "GMT"),
    `x-amz-bucket-region` = region,
    `content-type` = "application/xml",
    `transfer-encoding` = "chunked",
    server = "AmazonS3",
    class = "s3_bucket",
    Name = bucket,
    Prefix = prefix,
    Marker = list(),
    MaxKeys = as.character(max_keys),
    IsTruncated = tolower(as.character(truncated)),
    CommonPrefixes = character()
  )
}
