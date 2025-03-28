# Bayesian spatially-temporally varying coefficients GLM using predictive stacking

This repository contains code to implement different analyses, as it appears 
in the manuscript "Bayesian Inference for Spatial-temporal Non-Gaussian Data 
Using Predictive Stacking".

Soumyakanti Pan, Lu Zhang, Jonathan R. Bradley, and Sudipto Banerjee. 2024. 
_Bayesian Inference for Spatial-temporal Non-Gaussian Data Using Predictive Stacking._ https://arxiv.org/abs/2406.04655.

***

>[!IMPORTANT]
> **Quick start**: Download and view the file 'run/vignette.html'. 
To run example code, 
1. Clone repository (see below)
2. Open terminal and go to the directory 'run'
3. Issue the command 'make'

***

To reproduce results or to try out the example code, issue
```bash
git clone https://github.com/SPan-18/stvcGLMstack.git
```
on your personal computer or, remote server to clone this repository.

To run the scripts, one has to download the development version of the R package 
`spStack`. Please find below instructions on how to install the package.

The directory `data` contains synthetic datasets used in different simulation 
experiments as well as a dataset on migrant bird sightings from the [North 
American Breeding Bird Survey](https://www.usgs.gov/data/2022-release-north-american-breeding-bird-survey-dataset-1966-2021)
(2010-19). The directory `src` contains some R functions required to implement 
our algorithm. The directory `run` contains executable `.R` scripts.

## Installing `spStack`
For a quick installation of the development version, run the following command in R.
```r
# Install development version from GitHub
# install.packages("pak")
pak::pak("SPan-18/spStack-dev")
```

### Installing `spStack` from source
To install the package from source, download the tarball 
*spStack_X.X.XX.tar.gz* file. After setting the working directory at the file 
location, either issue `R CMD install spStack_X.X.XX.tar.gz` in the terminal, 
or run the following command in R to install the package.
```r
install.packages("spStack_X.X.XX.tar.gz", type = "source", repos = NULL)
```

Note that the package is written in C++ with calls to FORTRAN routines and hence 
contains a `Makevars` file for cross-platform portability. So, it is important 
to set the correct path to FORTRAN libraries as well as BLAS and LAPACK on your 
computer. For example, if you are working on MacOS, create a file `~.R/Makevars` 
and set global configurations for the libraries to link with R. The following is 
an example of such a Makevars file.
```bash
# Set Fortran library paths
FLIBS = -L/opt/homebrew/opt/gcc/lib/gcc/14 -lgfortran -lquadmath -lm

# BLAS and LAPACK libraries (using Accelerate framework on macOS)
BLAS_LIBS = -L/System/Library/Frameworks/Accelerate.framework/Versions/Current/ -framework Accelerate
LAPACK_LIBS = -L/System/Library/Frameworks/Accelerate.framework/Versions/Current/ -framework Accelerate
```
It tells R to use the Accelerate framework, that comes pre-installed with Mac 
for BLAS and LAPACK functions. If you do not have `gfortran`, simply run 
`brew install gcc` on the terminal which will install the `gcc` compiler and 
`gfortran` comes bundled with `gcc`. If `gcc` is installed using Homebrew, then 
the path should be the same as above, otherwise the path for `gfortran` needs to 
set correctly.

Once successfully installed, load the library in R.
```r
library(spStack)
```
