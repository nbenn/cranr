test_that("s3 sync", {

  rem <- withr::local_tempdir()

  pkg1 <- pkgbuild::build(
    system.file("testdata", "pkg1", package = "cranr"),
    withr::local_tempdir(),
    quiet = TRUE
  )

  init_repo(rem)
  insert_pkg(pkg1, rem)

  Sys.sleep(1)

  loc <- withr::local_tempdir()

  pkg2 <- pkgbuild::build(
    system.file("testdata", "pkg2", package = "cranr"),
    withr::local_tempdir(),
    quiet = TRUE
  )

  init_repo(loc)
  insert_pkgs(c(pkg1, pkg2), loc)

  mk <- mockthat::local_mock(
    `aws.s3::get_bucket` = function(...) {
      s3_bucket(rem)
    },
    `aws.s3::delete_object` = function(object, ...) {
      file <- file.path(rem, object)
      file.exists(file) && unlink(file) == 0
    },
    `aws.s3::put_object` = function(file, object, ...) {
      file.copy(file, file.path(rem, object), overwrite = TRUE)
    }
  )

  existing <- get_keys()

  expect_type(existing, "character")
  expect_type(get_md5(), "character")
  expect_length(get_md5(), length(existing))

  expect_true(basename(pkg1) %in% basename(existing))
  expect_false(basename(pkg2) %in% basename(existing))

  expect_message(
    expect_warning(rm_keys("foo"), "were not deleted"),
    "removing keys"
  )

  expect_snapshot(upload_repo(loc))

  expect_true(basename(pkg2) %in% basename(get_keys()))
})
