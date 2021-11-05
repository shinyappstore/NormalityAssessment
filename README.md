
# NormalityAssessment

A graphical user interface for testing normality visually

<!-- start badges -->

![](https://img.shields.io/badge/release-v0.0.1-blue?style=flat) [![CRAN
status](https://www.r-pkg.org/badges/version/NormalityAssessment)](https://CRAN.R-project.org/package=NormalityAssessment)
<!-- end badges -->

## Description

The `NormalityAssessment` `R` package includes an interactive Shiny
application, which is run locally on the user’s machine. It enables the
creation of normal quantile-quantile (QQ) plots and histograms for
assessing normality. The methods implemented are based on recent
developments made in graphical inference. In the app, the features in
the ‘Explore Simulated Data’ tab enable the user to run the Rorschach
procedure, and those in the ‘Include Your Data’ tab allow the user to
run the line-up procedure. Details on these two procedures can be found
in the articles included in the References section below.

## Installation

The `NormalityAssessment` package can be installed from either
<a href="https://cran.r-project.org/" target="_blank">CRAN</a> or
<a href="https://github.com" target="_blank">GitHub</a>.

#### Installing from CRAN

To install from CRAN, run the following code in `R`:

``` r
install.packages("NormalityAssessment")
```

#### Installing from GitHub

To install the package from GitHub, run the following code in `R`:

``` r
install.packages("remotes")  # installs the remotes package for accessing the install_github() function
remotes::install_github("ccasement/NormalityAssessment")  # installs the NormalityAssessment package
```

## Usage

The `NormalityAssessment` application can be run using a single line of
code in `R`:

``` r
NormalityAssessment::runNormalityAssessmentApp()
```

## References

Buja, A., Cook, D., Hofmann, H., Lawrence, M., Lee, E. K., Swayne, D.
F., & Wickham, H. (2009). Statistical inference for exploratory data
analysis and model diagnostics. Philosophical Transactions of the Royal
Society of London A: Mathematical, Physical and Engineering Sciences,
367(1906), 4361-4383.

Wickham, H., Cook, D., Hofmann, H., & Buja, A. (2010). Graphical
inference for infovis. IEEE Transactions on Visualization and Computer
Graphics, 16(6), 973-979.

## Bug Reporting

If you happen to find any bugs, we kindly ask that you email us at
<casementc@gmail.com>.

## License

`NormalityAssessment` is distributed under the MIT license. For details,
see the LICENSE.md file.
