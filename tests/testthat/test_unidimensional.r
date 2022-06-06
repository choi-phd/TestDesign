require(mirtCAT)

theta_grid <- expand.grid(
  seq(-3, 3, 1)
)
theta_grid <- as.matrix(theta_grid)
nq <- dim(theta_grid)[1]

test_that("unidimensional items return expected values (1PL)", {

  par_grid <- expand.grid(
    a1 = 1,
    d  = seq(-4, 4, 2)
  )
  par_grid <- as.matrix(par_grid)
  np <- dim(par_grid)[1]

  for (p in 1:np) {

    pars <- par_grid[p, , drop = FALSE]
    pars <- do.call(rbind, replicate(10, pars, simplify = FALSE))

    item <- generate.mirt_object(
      parameters = pars,
      itemtype = "Rasch"
    )
    item <- mirt::extract.item(item, 1)
    pars <- pars[1, ]

    if (pars[1] == 0) {
      next
    }

    p_expected <- mirt::probtrace(item, theta_grid)[, 2]
    p_obtained <- TestDesign:::array_p_1pl(
      theta_grid,
      -(pars[2] / pars[1])
    )[, 1]
    expect_equal(p_expected, p_obtained)

    info_expected <- mirt::iteminfo(item, theta_grid)
    info_obtained <- TestDesign:::array_info_1pl(
      theta_grid,
      -(pars[2] / pars[1])
    )[, 1]
    expect_equal(info_expected, info_obtained)

  }

})

test_that("unidimensional items return expected values (2PL)", {

  par_grid <- expand.grid(
    a1 = seq(0, 2, .5),
    d  = seq(-4, 4, 2)
  )
  par_grid <- as.matrix(par_grid)
  np <- dim(par_grid)[1]

  for (p in 1:np) {

    pars <- par_grid[p, , drop = FALSE]
    pars <- do.call(rbind, replicate(10, pars, simplify = FALSE))

    item <- generate.mirt_object(
      parameters = pars,
      itemtype = "2PL"
    )
    item <- mirt::extract.item(item, 1)
    pars <- pars[1, ]

    if (pars[1] == 0) {
      next
    }

    p_expected <- mirt::probtrace(item, theta_grid)[, 2]
    p_obtained <- TestDesign:::array_p_2pl(
      theta_grid,
      pars[1],
      -(pars[2] / pars[1])
    )[, 1]
    expect_equal(p_expected, p_obtained)

    info_expected <- mirt::iteminfo(item, theta_grid)
    info_obtained <- TestDesign:::array_info_2pl(
      theta_grid,
      pars[1],
      -(pars[2] / pars[1])
    )[, 1]
    expect_equal(info_expected, info_obtained)

  }

})

test_that("unidimensional items return expected values (3PL)", {

  par_grid <- expand.grid(
    a1 = seq(0, 2, .5),
    d  = seq(-4, 4, 2),
    g  = seq(0, 0.5, .25)
  )
  par_grid <- as.matrix(par_grid)
  np <- dim(par_grid)[1]

  for (p in 1:np) {

    pars <- par_grid[p, , drop = FALSE]
    pars <- do.call(rbind, replicate(10, pars, simplify = FALSE))

    item <- generate.mirt_object(
      parameters = pars,
      itemtype = "3PL"
    )
    item <- mirt::extract.item(item, 1)
    pars <- pars[1, ]

    if (pars[1] == 0) {
      next
    }

    p_expected <- mirt::probtrace(item, theta_grid)[, 2]
    p_obtained <- TestDesign:::array_p_3pl(
      theta_grid,
      pars[1],
      -(pars[2] / pars[1]),
      pars[3]
    )[, 1]
    expect_equal(p_expected, p_obtained)

    info_expected <- mirt::iteminfo(item, theta_grid)
    info_obtained <- TestDesign:::array_info_3pl(
      theta_grid,
      pars[1],
      -(pars[2] / pars[1]),
      pars[3]
    )[, 1]
    expect_equal(info_expected, info_obtained)

  }

})

test_that("unidimensional items return expected values (PC)", {

  par_grid <- expand.grid(
    a1 = 1,
    d1 = seq(-4, 4, 2),
    d2 = seq(-4, 4, 2)
  )
  par_grid <- as.matrix(par_grid)
  np <- dim(par_grid)[1]

  for (p in 1:np) {

    pars <- par_grid[p, , drop = FALSE]
    pars <- do.call(rbind, replicate(10, pars, simplify = FALSE))

    item <- generate.mirt_object(
      parameters = pars,
      itemtype = "gpcm"
    )
    ab_pars <- coef(item, IRTpars = TRUE)[[1]]
    item <- mirt::extract.item(item, 1)

    p_expected <- mirt::probtrace(item, theta_grid)
    p_obtained <- TestDesign:::array_p_pc(
      theta_grid,
      ab_pars[2:3]
    )
    expect_equal(sum((p_obtained - p_expected) ** 2), 0, tolerance = 1e-16)

    info_expected <- mirt::iteminfo(item, theta_grid)
    info_obtained <- TestDesign:::array_info_pc(
      theta_grid,
      ab_pars[2:3]
    )[, 1]
    expect_equal(info_expected, info_obtained)

  }

})

test_that("unidimensional items return expected values (GPC)", {

  par_grid <- expand.grid(
    a1 = seq(0.5, 2.5, 1),
    d1 = seq(-4, 4, 2),
    d2 = seq(-4, 4, 2)
  )
  par_grid <- as.matrix(par_grid)
  np <- dim(par_grid)[1]

  for (p in 1:np) {

    pars <- par_grid[p, , drop = FALSE]
    pars <- do.call(rbind, replicate(10, pars, simplify = FALSE))

    item <- generate.mirt_object(
      parameters = pars,
      itemtype = "gpcm"
    )
    ab_pars <- coef(item, IRTpars = TRUE)[[1]]
    item <- mirt::extract.item(item, 1)

    p_expected <- mirt::probtrace(item, theta_grid)
    p_obtained <- TestDesign:::array_p_gpc(
      theta_grid,
      ab_pars[1],
      ab_pars[2:3]
    )
    expect_equal(sum((p_obtained - p_expected) ** 2), 0, tolerance = 1e-16)

    info_expected <- mirt::iteminfo(item, theta_grid)
    info_obtained <- TestDesign:::array_info_gpc(
      theta_grid,
      ab_pars[1],
      ab_pars[2:3]
    )[, 1]
    expect_equal(info_expected, info_obtained)

  }

})

test_that("unidimensional items return expected values (GR)", {

  par_grid <- expand.grid(
    a1 = seq(0.5, 2.5, 1),
    d1 = seq(0, 4, 1.5),
    d2 = seq(-4, 0, 1.5)
  )
  par_grid <- as.matrix(par_grid)
  np <- dim(par_grid)[1]

  for (p in 1:np) {

    pars <- par_grid[p, , drop = FALSE]
    pars <- do.call(rbind, replicate(10, pars, simplify = FALSE))

    item <- generate.mirt_object(
      parameters = pars,
      itemtype = "graded"
    )
    ab_pars <- coef(item, IRTpars = TRUE)[[1]]
    item <- mirt::extract.item(item, 1)

    p_expected <- mirt::probtrace(item, theta_grid)
    p_obtained <- TestDesign:::array_p_gr(
      theta_grid,
      ab_pars[1],
      ab_pars[2:3]
    )
    expect_equal(sum((p_obtained - p_expected) ** 2), 0, tolerance = 1e-16)

    info_expected <- mirt::iteminfo(item, theta_grid)
    info_obtained <- TestDesign:::array_info_gr(
      theta_grid,
      ab_pars[1],
      ab_pars[2:3]
    )[, 1]
    expect_equal(info_expected, info_obtained)

  }

})
