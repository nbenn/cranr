test_that("install pkg1", {

  dir <- withr::local_tempdir()

  expect_length(list.files(dir), 0L)

  expect_null(
    expect_invisible(init_repo(dir))
  )

  expect_true(all(c("index.html", "src") %in% list.files(dir)))

  expect_identical(list.files(file.path(dir, "src")), "contrib")
  expect_length(list.files(file.path(dir, "src", "contrib")), 0L)

  pkg <- pkgbuild::build(
    system.file("testdata", "pkg1", package = "cranr"),
    withr::local_tempdir(),
    quiet = TRUE
  )

  expect_null(
    expect_invisible(insert_pkg(pkg, dir))
  )

  ctb <- file.path(dir, "src", "contrib")

  expect_true(
    all(c("PACKAGES", "PACKAGES.gz", "PACKAGES.rds") %in% list.files(ctb))
  )
})
