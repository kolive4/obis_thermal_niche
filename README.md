obis_thermal_niche
================

R tool for assessing uncertainties around qualities of observation for environmental niches using OBIS data.

## Requirements

-   [R v4.1](https://www.r-project.org/)
-   [rlang](https://CRAN.R-project.org/package=rlang)
-   [dplyr](https://CRAN.R-project.org/package=dplyr)
-   [readr](https://CRAN.R-project.org/package=readr)
-   [ggplot2](https://CRAN.R-project.org/package=ggplot2)
-   [robis](https://CRAN.R-project.org/package=robis)
-   [rappdirs](https://CRAN.R-project.org/package=rappdirs)
-   [mapr](https://CRAN.R-project.org/package=mapr)
-   [tidytable](https://CRAN.R-project.org/package=tidytable)

## Installation

    remotes::install_github("kolive4/obis_thermal_niche")
    
## Usage

Retrieve a data set by species.

``` r
suppressPackageStartupMessages({
  library(obisniche)
  library(dplyr)
})

# make it quiet for th epurpose of the README markdown
# x = fetch_obis(species = "Carcharodon carcharias",
#                verbose = FALSE, progress = FALSE)  |>
#   dplyr::glimpse()
```

If you have downloaded multiple datasets you can list the ones you have.

``` r
(spp < list_obis())
```

Now that you have a set of data files you can read them, optionally with
a select number of [OBIS Darwin 
Core] (https://manual.obis.org/darwin_core.html) required
and recommended fields.

``` r
x <- read_obis(spp[1], dwc = TRUE)
glimpse(x)
```

And plot

``` r
plot_obis(x, what = 'ggplot')
```





