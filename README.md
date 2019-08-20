# outviz
[![Build Status](https://travis-ci.org/pbsag/outviz.svg?branch=master)](https://travis-ci.org/pbsag/outviz)

`outviz` is a R package that [Greg Macfarlane](mailto:gregmacfarlane@byu.edu) and [Kyle Ward](mailto:kyle@caliper.com) built for building summary tables and output visualizations for transport planning models. The only documentation at this point are the man pages built from [roxygen2 headers](https://kbroman.org/pkg_primer/pages/docs.html) in the code. There are no vignettes or wiki content yet, although both will become works in progress if this packages works out in current use.

In addition to needing a better package name `outviz` requires `plotly` and `chorddiag` to be installed. Both packages should be installed as part of building the package locally. If the installation stalls, as it did for me, you can run `install.packages("plotly")` to install it from CRAN. `chorddiag` is an [interface to the D3 library for generating chord diagrams](https://github.com/mattflor/chorddiag), and must be installed from its GitHub repository:
```r
install.packages("devtools")  # If not already installed
devtools::install_github("mattflor/chorddiag")
```

The package appears to export the following functions:

+ `chord_chart` creates an HTML chord diagram from matrix information
+ `cross_table` builds an attractive cross-tabulation
+ `cut_volumes` cuts volumes into intervals
+ `link_measures_table` returns a table that has yet to be investigated (and probably needs a more descriptive name)
+ `link_stats_table` returns a table with link validation statistics
+ `link_targets` returns the percent of links within a user-defined target range
+ `mdd_table` returns a table with maximum desirable deviation statistics
+ `numerify` "numerifies" a mixed vector by stripping non-numeric values from each element
+ `pct_error` returns percent error
+ `pct_rmse` returns the percent root mean squared error
+ `plot_mdd` creates a maximum desirable deviation plot
+ `plot_validation` creates a link validation plot
+ `plotly_mdd` creates a maximum desirable deviation Plotly object
+ `plotly_validation` creates link validation plot as a Plotly object
+ `plotly_tlfd` plots a trip length frequency distribution comparison as an interactive figure
+ `prep_tlfd_data` prepares model data for plotting from skim and trip matrices for input to `plotly_tlfd`, as well as average impedance and intrazonal percentage summaries
+ `rmse` returns the root mean squared error 

A quick look at the contents of `R/data.R` reveals that the following link fields are required:

+ `a` node identifier
+ `b` node identifier
+ `facility_type` of the links
+ `facility_group` is a higher-level aggregation of `facility type`s
+ `area_type` of the links
+ `area_name` is a label corresponding to `area_type`
+ `volume` is the assigned flow on the link
+ `screenline` the link is associated with
+ `count` for the calibration year
+ `distance` in miles
+ `capacity` of the link, presumably in same time interval as the `volume`
+ `ffspeed` is the free-flow speed on the link, in miles/hour
+ `speed` is the simulated speed from the network assignment, in miles/hour

It appears that other fields can be coded as well and will be ignored by `outviz`.
