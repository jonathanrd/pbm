# PBM - Protein Binding Models

The goal of PBM is to make analysing Biolayer Interferometry (BLI) or Surface Plasmon Resonance (SPR) data more open. After initial binding parameters are known, binding curves can be simulated and parameters such as: analyte concentration, time of association, dissociation etc. can be varied. The models within this package may also be used to fit a curve to measured binding data using a non-linear regression.

Currently, two binding models are included with this package:

 - 1:1 binding
 - 2:1 heterogeneous binding.

 *Note: more binding models are on their way.. bivalent model, mass-transport params etc.*

## Installation

Install the released version of usethis from CRAN:

``` r
install.packages("pbm")
```

Or install the development version from GitHub with:

``` r
install.packages("devtools")
devtools::install_github("jonathanrd/pbm")
```

## Usage

For usage information and examples, see the vignette and reference manual on CRAN: [pbm](https://cran.r-project.org/package=pbm).
