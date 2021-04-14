# localXCR

## Installation

To use localXCR you need to have an R installation - the installer can be downloaded [here](https://cloud.r-project.org).

Once R is installed you can install the local XCR R package by using the following commands in the R console (indicated with a `>`)

```R
install.packages('devtools')
devtools::install_github('tjgorrie/nglShiny')
devtools::install_github('tjgorrie/localXCR')
```

## Usage

```R
library(localXCR)
launch_XCR()
```

This will launch a browser tab that is running XCR

## Preparing data for XCR

## Tips and Tricks?

## Why is this written in R instead of python?

Python and R each have their own limitations - although R is perhaps out-of-field for structural biology, python's main limitation is that I do not know it that well thus I have used R instead!
