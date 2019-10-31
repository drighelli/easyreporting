# easyreporting
easyreporting

[![Project Status: Active - The project has reached a stable, usable state and is being actively developed.](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)

This package implements an R6 class for facilitating the automation of creation of
 rmarkdown files inside other packages/software.
 

## Installation

At the current status, the easyiest way to install it is through devtools:

```{r}
if (!requireNamespace("devtools", quietly = TRUE))
    install.packages("devtools")
    
devtools::install_github("drighelli/easyreporting", build_vignettes=TRUE)
```

## Long documentations

The easyreporting package comes with two vignettes to guide the user on its usage:

### Bioinformatics usage
The first one has been designed for illustrating the easy way to implement and use  easyreporting during bioinformatics analysis.

```{r}
vignette("bio_usage", package="easyreporting")
```

### Standard usage

This one is for illustrating a standard usage of the package showing all its multiple functionalities.

```{r}
vignette("standard_usage", package="easyreporting")
```


