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

set.seed(1)
true_theta <- seq(-2, 2, 1)
resp <- simResp(itempool_reading, true_theta)

test_that("excluding stimuli works", {

  skip_on_cran() # lpsymphony causes ASAN heap-buffer-overflow
  skip_if_not_installed("Rsymphony")

  cfg <- createShadowTestConfig(
    MIP = list(solver = "Rsymphony")
  )

  set.seed(1)
  exclude <- lapply(
    1:5,
    function(x) {
      tmp <- list()
      tmp$i <- sample(itempool_reading@id, 30)
      tmp$s <- sample(constraints_reading@st_attrib@data$STID, 5)
      return(tmp)
    }
  )

  solution <- Shadow(
    cfg, constraints_reading, true_theta, data = resp,
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

  administered_stimuli <- lapply(
    solution@output,
    function(x) {
      o <- x@administered_stimulus_index
      o <- solution@constraints@st_attrib@data$STID[o]
      return(o)
    }
  )

  o <- mapply(
    function(intent, observed) {
      any(intent$s %in% observed)
    },
    exclude,
    administered_stimuli,
    SIMPLIFY = FALSE
  )

  o <- any(unlist(o))
  expect_equal(o, FALSE)

})
