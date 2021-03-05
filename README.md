Simulation Examples for Epi 2 Lecture
================
Aaron Miller
3/4/2021

## Simulation Examples

This repository contains R scripts and example data sets for the
simulation examples presented during the Epidemiology II course for the
Spring 2021 course.

The following examples are contained in the `R` script folder:

-   **monte\_carlo\_pi.R** - This script contains an example of a Monte
    Carlo Simulation used to estimate the value of pi

-   **sample\_size\_body\_temperature.R** - This script contains an
    example of using empirical body temperature measurements to perform
    a power analysis (i.e., determine required sample size) for a study
    involving the use of body temperature measurements

-   **cftr\_simulation.R** - This script contains examples of
    simulations that were performed for [this
    study](https://www.pnas.org/content/117/3/1621), where the risk for
    multiple contains associated with CF carrier state was evaluated

    -   **cftr\_functions.R** - This script contains functions that are
        used for the CFTR simulation; these functions are called by the
        cftr\_simulation.R script

The following example datasets are provided in the `data` folder:

-   **cftr\_data.RData** - Simulated patient data for the CFTR analysis
    (note: this data is not real but should behave similarly to that
    used in the study)

-   **cftr\_simulation\_results.RData** - some of the results data from
    the CFTR study. This data is needed for some of the simulation
    analysis.

-   **temperature\_episodes.RData** - Simulated body temperature
    readings for different individuals recorded at different times
    (note: this data is not real, but should behave similarly to data
    that has been used in prior investigations)
