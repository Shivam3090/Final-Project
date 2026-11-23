# Texas Housing Market | Interactive R Shiny Dashboard

**R | Shiny | shinydashboard | ggplot2 | Plotly | dplyr**

An interactive dashboard for exploring historical housing prices, sales, and inventory across Texas cities using the `ggplot2::txhousing` dataset.

## Business problem
How do median housing prices, sales activity, and inventory patterns differ by city and time period?

## Features
- City, year-range, and month filters
- KPI cards for median price, sales, and inventory
- Median-price trends
- Sales and listings comparisons
- Sales-versus-price analysis
- Cross-city median price comparisons

## Run locally
Install the required R packages, then open and run `H_Shiny DB.R` in RStudio:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "shinydashboard", "plotly", "tidyr"))
shiny::runApp("H_Shiny DB.R")
```

## Data and scope
This analysis uses historical data bundled with ggplot2; it is not a current housing-market forecast. Filtering to one selected month may leave few time-series observations.

## Security note
Deployment tokens, secrets, and machine-specific file paths must never be committed. Previously exposed shinyapps.io deployment credentials must be revoked or rotated, and Git history reviewed.

[Explore more projects](https://github.com/Shivam3090)
