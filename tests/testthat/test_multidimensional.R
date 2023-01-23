require(mirtCAT)

theta_grid <- expand.grid(
  seq(-3, 3, 3),
  seq(-3, 3, 3)
)
theta_grid <- as.matrix(theta_grid)
nq <- dim(theta_grid)[1]

test_that("multidimensional items return expected values (2PL)", {

  par_grid <- expand.grid(
    a1 = seq(0, 2, .5),
    a2 = seq(0, 2, .5),
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

    p_expected <- mirt::probtrace(item, theta_grid)[, 2]
    p_obtained <- TestDesign:::array_p_m_2pl(
      theta_grid,
      pars[1:2],
      pars[3]
    )[, 1]
    expect_equal(p_expected, p_obtained)

    info_expected <- mirt::iteminfo(item, theta_grid, multidim_matrix = TRUE)
    info_obtained <- TestDesign:::array_info_m_2pl(
      theta_grid,
      pars[1:2],
      pars[3]
    )
    expect_equal(info_expected, info_obtained)

    angle_radians <- TestDesign:::a_to_alpha(pars[1:2])
    angle_degrees <- angle_radians / (2 * pi) * 360
    if (any(is.nan(angle_degrees))) {
      next
    }

    dirinfo_expected <- mirt::iteminfo(item, theta_grid, angle_degrees)
    dirinfo_obtained <- TestDesign:::array_dirinfo_m_2pl(
      theta_grid,
      pars[1:2],
      pars[3]
    )[, 1]
    expect_equal(dirinfo_expected, dirinfo_obtained)

  }

})

test_that("multidimensional items return expected values (3PL)", {

  par_grid <- expand.grid(
    a1 = seq(0, 2, .5),
    a2 = seq(0, 2, .5),
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

    p_expected <- mirt::probtrace(item, theta_grid)[, 2]
    p_obtained <- TestDesign:::array_p_m_3pl(
      theta_grid,
      pars[1:2],
      pars[3],
      pars[4]
    )[, 1]
    expect_equal(p_expected, p_obtained)

    info_expected <- mirt::iteminfo(item, theta_grid, multidim_matrix = TRUE)
    info_obtained <- TestDesign:::array_info_m_3pl(
      theta_grid,
      pars[1:2],
      pars[3],
      pars[4]
    )
    expect_equal(info_expected, info_obtained)

    angle_radians <- TestDesign:::a_to_alpha(pars[1:2])
    angle_degrees <- angle_radians / (2 * pi) * 360
    if (any(is.nan(angle_degrees))) {
      next
    }

    dirinfo_expected <- mirt::iteminfo(item, theta_grid, angle_degrees)
    dirinfo_obtained <- TestDesign:::array_dirinfo_m_3pl(
      theta_grid,
      pars[1:2],
      pars[3],
      pars[4]
    )[, 1]
    expect_equal(dirinfo_expected, dirinfo_obtained)

  }

})

test_that("multidimensional items return expected values (GPC)", {

  par_grid <- expand.grid(
    a1 = seq(0.5, 2.5, 1),
    a2 = seq(0.5, 2.5, 1),
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
    item <- mirt::extract.item(item, 1)
    pars <- pars[1, ]

    p_expected <- mirt::probtrace(item, theta_grid)
    p_obtained <- TestDesign:::array_p_m_gpc(
      theta_grid,
      pars[1:2],
      pars[3:4]
    )
    expect_equal(sum((p_obtained - p_expected) ** 2), 0, tolerance = 1e-16)

    info_expected <- mirt::iteminfo(item, theta_grid, multidim_matrix = TRUE)
    info_obtained <- TestDesign:::array_info_m_gpc(
      theta_grid,
      pars[1:2],
      pars[3:4]
    )
    expect_equal(info_expected, info_obtained, tolerance = 1e-7)

    angle_radians <- TestDesign:::a_to_alpha(pars[1:2])
    angle_degrees <- angle_radians / (2 * pi) * 360
    if (any(is.nan(angle_degrees))) {
      next
    }

    dirinfo_expected <- mirt::iteminfo(item, theta_grid, angle_degrees)
    dirinfo_obtained <- TestDesign:::array_dirinfo_m_gpc(
      theta_grid,
      pars[1:2],
      pars[3:4]
    )[, 1]
    expect_equal(dirinfo_expected, dirinfo_obtained)

  }

})

test_that("multidimensional items return expected values (GR)", {

  par_grid <- expand.grid(
    a1 = seq(0.5, 2.5, 1),
    a2 = seq(0.5, 2.5, 1),
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
    item <- mirt::extract.item(item, 1)
    pars <- pars[1, ]

    p_expected <- mirt::probtrace(item, theta_grid)
    p_obtained <- TestDesign:::array_p_m_gr(
      theta_grid,
      pars[1:2],
      pars[3:4]
    )
    expect_equal(sum((p_obtained - p_expected) ** 2), 0, tolerance = 1e-16)

    info_expected <- mirt::iteminfo(item, theta_grid, multidim_matrix = TRUE)
    info_obtained <- TestDesign:::array_info_m_gr(
      theta_grid,
      pars[1:2],
      pars[3:4]
    )
    expect_equal(info_expected, info_obtained)

    angle_radians <- TestDesign:::a_to_alpha(pars[1:2])
    angle_degrees <- angle_radians / (2 * pi) * 360
    if (any(is.nan(angle_degrees))) {
      next
    }

    dirinfo_expected <- mirt::iteminfo(item, theta_grid, angle_degrees)
    dirinfo_obtained <- TestDesign:::array_dirinfo_m_gr(
      theta_grid,
      pars[1:2],
      pars[3:4]
    )[, 1]
    expect_equal(dirinfo_expected, dirinfo_obtained)

  }

})
