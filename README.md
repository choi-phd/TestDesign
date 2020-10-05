
# TestDesign

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/choi-phd/TestDesign.svg?branch=master)](https://travis-ci.com/choi-phd/TestDesign)
[![Number of
downloads](https://cranlogs.r-pkg.org/badges/grand-total/TestDesign?color=lightgrey)](https://cran.r-project.org/package=TestDesign)
<!-- badges: end -->

Optimal **Test Design** Approach to Fixed and Adaptive Test Construction

Founded on the optimal test design framework, the `TestDesign` package
allows for constructing fixed test forms and simulating adaptive tests
to the same test specifications with complex sets of constraints. This
approach can render a variety of testing formats with different levels
of adaptativeness and relative efficiency. Several item banks from
real-world testing scenarios are provided as examples.

## Installation

Install the latest release (1.1.3) from CRAN:

``` r
install.packages("TestDesign")
```

## Quick start

``` r
itempool    <- loadItemPool(itempool_science_data)
itemattrib  <- loadItemAttrib(itemattrib_science_data, itempool)
constraints <- loadConstraints(constraints_science_data, itempool, itemattrib)
```

## Static (fixed-form) assembly

``` r
cfg <- createStaticTestConfig(
  item_selection = list(
    method          = "TIF",
    target_location = c(-1,  1),
    target_value    = c( 8, 10)
  )
)

solution <- Static(cfg, constraints)

summary(solution)
print(solution)
plot(solution)
```

## Shadow (adaptive) assembly

``` r
cfg <- createShadowTestConfig()

solution <- Shadow(cfg, constraints, true_theta = c(0, 1))

plot(solution, type = "audit" , examinee_id = 1)
plot(solution, type = "shadow", examinee_id = 2, simple = T)
summary(solution)
```

The documentation is available at
(<https://choi-phd.github.io/TestDesign>)
