# Fertility Rate Explainer

This analysis spawned from my work looking at how fertility rates are communicated. It uses data from the [Human Fertility Database](http://humanfertility.org/) and [Human Mortality Database](mortality.org). You will have to register for and supply credentials for both to get the data, only slightly modifying my code. 

The script will:
  1. Download the necessary data from HMD and HFD into an input folder in the project directory, creating it if necessary.
  2. Build the frames for an animated TFR-ASFR chart.
  3. Fit a fertility forecast model using the [demography R package](https://cran.r-project.org/web/packages/demography/demography.pdf)
  4. Build a chart of observed rates for select cohorts.
  5. Build a chart of the forecasted ASFR for the 1990 cohort