## -----------------------------------------------------------------------------
library(ramp.xds)
library(ramp.func)

## -----------------------------------------------------------------------------
devtools::load_all("~/git/ramp.xds")

## -----------------------------------------------------------------------------
mod <- xds_setup(nPatches=5, residence = c(1,2,3,4), searchB = c(4:1))
get_blood_search_weights(mod)

## -----------------------------------------------------------------------------
mod <- setup_blood_search_weights(c(2:5), mod) 
get_blood_search_weights(mod)

## -----------------------------------------------------------------------------
mod <- setup_blood_search_weights("as_is", mod, list(weights=c(2:5))) 
get_blood_search_weights(mod)

## ----eval=F-------------------------------------------------------------------
# setup_blood_search_weights(c(2:4), mod)

## -----------------------------------------------------------------------------
mod <- setup_blood_search_weights("random" , mod) 
get_blood_search_weights(mod)

## -----------------------------------------------------------------------------
library(ramp.demog)
devtools::load_all()

## -----------------------------------------------------------------------------
mod <- xds_setup(nPatches=5, residence = c(1,2,3,4), searchB = c(4:1))
cbind(get_H(mod), get_blood_search_weights(mod))

## -----------------------------------------------------------------------------
mod <- split_base_by_search_weights(c(.2, .3, .5), c(4, 1, .25), mod)
t(matrix(get_blood_search_weights(mod), 3, 4))

## -----------------------------------------------------------------------------
t(matrix(get_H(mod), 3, 4))

