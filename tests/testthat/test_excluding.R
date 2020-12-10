set.seed(1)
true_theta <- seq(-2, 2, 1)
resp <- simResp(itempool_science, true_theta)

test_that("excluding items works", {

  cfg <- createShadowTestConfig()

  set.seed(1)
  exclude <- lapply(
    1:5,
    function(x) {
      tmp <- list()
      tmp$i <- sample(itempool_science@id, 30)
      return(tmp)
    }
  )

  solution <- Shadow(
    cfg, constraints_science, true_theta, data = resp,
    exclude = exclude
  )

  administered_items <- lapply(
    solution@output,
    function(x) {
      o <- x@administered_item_index
      o <- solution@pool@id[o]
      return(o)
    }
  )

  o <- mapply(
    function(intent, observed) {
      any(intent$i %in% observed)
    },
    exclude,
    administered_items,
    SIMPLIFY = FALSE
  )

  o <- any(unlist(o))
  expect_equal(o, FALSE)

})
