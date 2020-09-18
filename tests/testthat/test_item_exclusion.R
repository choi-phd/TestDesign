set.seed(1)
true_theta <- seq(-2, 2, 1)
resp <- simResp(itempool_science, true_theta)

test_that("item exclusion works", {

  cfg <- createShadowTestConfig()

  set.seed(1)
  excluded_items <- lapply(
    1:5,
    function(x) {
      sample(itempool_science@id, 30)
    }
  )

  solution <- Shadow(
    cfg, constraints_science, true_theta, data = resp,
    excluded_items = excluded_items
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
      any(intent %in% observed)
    },
    excluded_items,
    administered_items,
    SIMPLIFY = FALSE
  )

  o <- any(unlist(o))
  expect_equal(o, FALSE)

})
