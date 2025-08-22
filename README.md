
<!-- README.md is generated from README.Rmd. Please edit that file -->

# moveEZ <img src="man/figures/logo.png" align="right" width="150"/>

<!-- badges: start -->

<!-- badges: end -->

The goal of `moveEZ` is to create animated biplots.

# Installation

You can install the development version of moveEZ from
[GitHub](https://github.com/) with:

``` r
library(devtools)
install_github("MuViSU/moveEZ")
```

# Background

Consider a dataset ${\mathbf{X}}$ comprising $n$ observations and $p$
continuous variables, along with an additional variable representing
“time.” This time variable need not correspond to chronological time; it
could just as well represent another form of ordered index, such as
algorithmic iterations or experimental stages.

A natural approach is to construct separate biplots for each level of
the time variable, enabling the user to explore how samples and variable
relationships evolve across time. However, when the time variable
includes many levels, this quickly results in an overwhelming number of
biplots.

This package addresses that challenge by animating a single biplot
across the levels of the time variable, allowing for dynamic
visualisation of temporal or sequential changes in the data.

The animation of the biplots—currently limited to PCA biplots—is based
on two conceptual frameworks:

1.  Fixed Variable Frame `moveplot()`: A biplot is first constructed
    using the full dataset ${\bf{X}}$, and the animation is achieved by
    slicing the observations according to the “time” variable. In this
    approach, the variable axes remain fixed, and only the sample points
    are animated over time.

2.  Dynamic Frame `moveplot2()` and `moveplot3()`: Separate biplots are
    constructed for each time slice of the data. Both the sample points
    and variable axes evolve over time, resulting in a fully dynamic
    animation that reflects temporal changes in the underlying data
    structure. The differences between these functions are highlighted
    in the subsequent sections.

Please have a look at the vignettes for a full illustration of how these
functions work.

# Still to Come!

We are actively working to develop and enhance the dynamic plotting
capabilities of these functions to expose and detect changes in
observations and variables over time.

Stay tuned for updates!

# Report Bugs and Support

If you encounter any issues or have questions, please open an issue on
the GitHub repository.
