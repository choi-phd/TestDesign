test_that("class-level validation for constraints works", {

  obj_names <- c(
    "constraints_science",
    "constraints_reading",
    "constraints_fatigue",
    "constraints_bayes"
  )

  tmp <- new("constraints")

  for (i in obj_names) {

    o <- eval(parse(text = sprintf("%s", i)))
    expect_true(validObject(o, test = TRUE) == TRUE)

    for (j in slotNames(o)) {
      oo       <- o

      if (!identical(
        slot(oo, j),
        slot(tmp, j))) {

        slot(oo, j) <- slot(tmp, j)
        expect_true(validObject(oo, test = TRUE) != TRUE)

      }

    }

  }

})
