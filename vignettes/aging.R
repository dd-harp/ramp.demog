## -----------------------------------------------------------------------------
library(ramp.xds)
library(ramp.library)

## ----fig.width=7, fig.height=4------------------------------------------------
cohort_model <- xds_setup_eir(Xname = "garki", eir = 1/365)
cohort_model <- xds_solve_cohort(cohort_model, Amax=3650, da=20)
xds_plot_X(cohort_model)

## -----------------------------------------------------------------------------
library(ramp.func)
library(ramp.forcing)
library(ramp.demog)

## -----------------------------------------------------------------------------
cohort_model <- change_season(makepar_F_sin(), cohort_model)
cohort_model <- change_age(makepar_F_type2(), cohort_model)

