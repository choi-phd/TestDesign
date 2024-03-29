% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/partitioning_functions.r
\docType{methods}
\name{Split}
\alias{Split}
\alias{Split,config_Static-method}
\title{Split an item pool into partitions}
\usage{
Split(
  config,
  constraints,
  n_partition,
  partition_type,
  partition_size_range = NULL,
  n_maximum_partitions_per_item = 1,
  force_solver = FALSE
)

\S4method{Split}{config_Static}(
  config,
  constraints,
  n_partition,
  partition_type,
  partition_size_range = NULL,
  n_maximum_partitions_per_item = 1,
  force_solver = FALSE
)
}
\arguments{
\item{config}{a \code{\linkS4class{config_Static}} object. Use \code{\link{createStaticTestConfig}} for this.}

\item{constraints}{a \code{\linkS4class{constraints}} object representing test specifications. Use \code{\link{loadConstraints}} for this.}

\item{n_partition}{the number of partitions to create.}

\item{partition_type}{\code{test} to create tests, or \code{pool} to create pools.}

\item{partition_size_range}{(optional) two integer values for the desired range for the size of a partition. Has no effect when \code{partition_type} is \code{test}.
For discrete item pools, the default partition size is (pool size / number of partitions).
For set-based item pools, the default partition size is (pool size / number of partitions) +/- smallest set size.}

\item{n_maximum_partitions_per_item}{(optional) the number of times an item can be assigned to a partition.
Setting this to 1 is equivalent to requiring all partitions to be mutually exclusive.
A caveat is that when this is equal to \code{n_partition}, the assembled partitions will be identical to each other,
because \code{\link{Split}} aims to minimize the test information difference between all partitions.
(default = \code{1})}

\item{force_solver}{if \code{TRUE}, do not check whether the solver is one of recommended solvers for complex problems (set-based assembly, partitioning). (default = \code{FALSE})}
}
\value{
\code{\link{Split}} returns an \code{\linkS4class{output_Split}} object containing item/set indices of created tests/pools.
}
\description{
\code{\link{Split}} is a function for splitting a pool into multiple parallel tests or pools.
When constructing parallel tests, each test is constructed to satisfy all constraints.
When constructing parallel pools, each pool is constructed so that it contains a test that satisfies all constraints.
}
\examples{
\dontrun{
config <- createStaticTestConfig(MIP = list(solver = "RSYMPHONY"))
constraints <- constraints_science[1:10]

solution <- Split(config, constraints, n_partition = 4, partition_type = "test"))
plot(solution)
solution <- Split(config, constraints, n_partition = 4, partition_type = "pool"))
plot(solution)
}
}
