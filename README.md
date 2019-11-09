# SSNAPStats

## Installation

``` r
# Install development version from GitHub
devtools::install_github("SSNAP/SSNAPStats")
```

Note that at present this is a private repository; to log in from the
command line you should set the GITHUB_PAT environment variable.
Refer to [Github PAT](https://github.com/settings/tokens) for more
information on setting this up. You can alternatively download and
build the package from the site.

## What is SSNAPStats?

SSNAPStats is an R package to take imported SSNAP data (currently
from CSV files); break them down into cohorts, and then calculate
indivdual or summary statistics from a large number of different
measures upon these cohorts. These results can then be used in
individual analysis, or used with the SSNAPReports and
SSNAPDashboard packages for producing performance reports and
real-time performance monitoring.

The data for SSNAP is hosted on the
[SSNAP website](http://www.strokeaudit.org). This also contains the
definitions for the measures.

A lot of optimisation is performed to reduce duplication and ensure
that each measure is performed once and once only. This helps to
produce faster results which is essential for realtime dashboarding.
It also reduces the maintenance costs.

The structure of SSNAPStats could be readily applied and genericised
to other national audits.

## How is the package structured?

SSNAPStats divides the audit into a set of cohort definitions (that
describe an audit) and a set of audit measures (that provide rules on
what we are measuring). Lists of audit measures are applied to a
cohort definition to produce a cohort. These functions can be applied
to any audit and are quite generic.

SSNAP-specific functions sit on top of these generic functions to
simplify the process by pre-defining commonly used cohorts and
audit measures. The \code{ssnap_cohorts} command provides all the
common cohort outputs with the least set-up. It also provides further
aggregation such as \code{ssnap_scores} which is an amalgamation of
various cohorts to produce the results for the scoring system used
by SSNAP to reflect performance.

SSNAPStats tries to minimise the processing to the fewest steps, so we
pre-parse it to work out which data will be shared between
calculations so we only fetch shared information once. We also try to
minimise the number of columns of data we need to only those either
asked for by the cohort, or needed to calculate a value in the
cohort. This is still under development and subject to more
optimisation.

The cohorts code performs a series of
[dplyr](https://dplyr.tidyverse.org) commands upon the dataset to
produce aggregated summary statistics. To maximise reusability, we
use [quasiquotation](https://adv-r.hadley.nz/quasiquotation.html) to 
separately store a list of all the possible measures for a given
cohort. We then have a set of internal cohorts lists which tell us
which measures are needed for a given cohort. This allows us to
easily create custom cohorts with any combination of measures in, or
to ensure single use when one cohort (e.g. a key indicators cohort)
only wants the median of a measure, whilst another (e.g. a portfolio)
wants all quartiles.

Quite often we can only calculate a summary statistic by performing
calculations on each row of a field (for example time differences);
we also do these in-line and store them in a list of fields. Ideally
we can do these in the future at database level on record locking to
improve our processing time.

## Contributing to this package

### Unit testing

Code should be checked in wherever possible with unit tests for
proving that the given portion of code works. This packages uses the
[TestThat](http://testthat.r-lib.org) package to produce unit tests.

If you find a bug within this code, it is good practice to firstly
write a unit test which proves the bug, add it to the unit tests, and
then fix the bug before proving that all the other unit tests pass
satisfactorily before checking it in.

Unit tests are run whenever you select Check. You can also run them
independently of the full check using Cmd-Shift-T (Mac) or
Ctrl-Shift-T (Windows).

### Code style guide

This code is written using the
[Tidyverse style guide](http://style.tidyverse.org) coding style. It
is checked using
[lintr](https://www.rdocumentation.org/packages/lintr/versions/0.2.0):


``` r
# Check your update meets the lintr style guide
lintr::lint_package
```

Check the output in the Markers tab of the Consule for any style
errors and correct them. A unit test also runs lintr: so your code
will fail the lintr unit test if a style error is found.

Larger code documents such as the list of ssnap measures in
ssnap_measures.R are broken up using code markers that allow
R studio to mark out the structure. 

### Documentation

Each key function (all external functions and major internal ones
are documented using
[Roxygen2](http://kbroman.org/pkg_primer/pages/docs.html). In
addition, a website for the package is created within the package
using [pkgdown](http://pkgdown.r-lib.org). To update the web pages
use:

``` r
# Update the package website documentation
pkgdown::build_site()
```
