item.1 <- new("item.1pl", difficulty = 0.5)
item.2 <- new("item.2pl", slope = 1.5, difficulty = 0.5)
item.3 <- new("item.3pl", slope = 1.5, difficulty = 0.5, guessing = 0.2)
item.4 <- new("item.pc", threshold = c(-0.5, 0.5), ncat = 3)
item.5 <- new("item.gpc", slope = 1.0, threshold = c(-0.5, 0.0, 0.5), ncat = 4)
item.6 <- new("item.gr", slope = 1.0, category = c(-1, 0, 1), ncat = 4)
item.names <- paste0("item.", 1:6)

for (i in item.names){
  fn.names <- c("calcProb", "calcDerivative", "calcDerivative2",
                "calcEscore", "calcFisher")
  for(fn in fn.names){
    test_that(fn, {
      tmp <- eval(parse(text = paste0(fn, "(", i, ", 0)[1]")))
      expect_true(is.numeric(tmp))
    })
  }

  fn.names <- c("calcHessian", "calcJacobian")
  for(fn in fn.names){
    test_that(fn, {
      tmp <- eval(parse(text = paste0(fn, "(", i, ", 0, 1)[1]")))
      expect_true(is.numeric(tmp))
    })
  }

  fn.names <- "calcLocation"
  for(fn in fn.names){
    test_that(fn, {
      tmp <- eval(parse(text = paste0(fn, "(", i, ")[1]")))
      expect_true(is.numeric(tmp))
    })
  }
}
