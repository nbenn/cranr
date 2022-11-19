test_that("install pkgs", {

  dir <- withr::local_tempdir()

  expect_length(list.files(dir), 0L)

  expect_null(
    expect_invisible(init_repo(dir))
  )

  expect_true(all(c("index.html", "src", "bin") %in% list.files(dir)))

  expect_identical(list.files(file.path(dir, "src")), "contrib")
  expect_length(list.files(file.path(dir, "src", "contrib")), 0L)

  pkg1 <- pkgbuild::build(
    system.file("testdata", "pkg1", package = "cranr"),
    withr::local_tempdir(),
    quiet = TRUE
  )

  expect_null(
    expect_invisible(insert_pkg(pkg1, dir))
  )

  ctb <- file.path(dir, "src", "contrib")

  expect_true(
    all(c("PACKAGES", "PACKAGES.gz", "PACKAGES.rds") %in% list.files(ctb))
  )

  pkg2 <- pkgbuild::build(
    system.file("testdata", "pkg2", package = "cranr"),
    withr::local_tempdir(),
    quiet = TRUE
  )

  expect_null(
    expect_invisible(insert_pkg(pkg2, dir))
  )

  expect_true(
    all(c("pkg1_0.0.1.tar.gz", "pkg2_0.0.1.tar.gz") %in% list.files(ctb))
  )
})
