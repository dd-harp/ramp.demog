## -----------------------------------------------------------------------------
library(ramp.xds)
library(ramp.func)
library(ramp.demog)

## -----------------------------------------------------------------------------
mod <- xds_setup(residence = c(1,1,2,2), nPatches=3) 
class(mod$XH_obj[[1]]$birth_obj)

## -----------------------------------------------------------------------------
Births(0, mod, 1)

## -----------------------------------------------------------------------------
birth_rates = 5:8
mod <- setup_births("constant", mod, list(birth_rates=birth_rates), i=1)
Births(0, mod, 1)

## -----------------------------------------------------------------------------
birth_rates = 9:12
mod <- setup_births("ts_func", mod, list(birth_rates=birth_rates, trend_par = makepar_F_exp(rate=0.03/365, N=4)), i=1)
Births(365, mod, 1)

