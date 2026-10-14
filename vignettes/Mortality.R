## ----eval=F-------------------------------------------------------------------
# male = c(30.1, 2.6, 0.85, 0.73, 1.4, 2.2, 6.5)
# female = c(23.1, 2.2, 0.68, 0.55, 0.9, 1.3, 4.3)
# breaks = c(0, 1, 5, 10, 15, 20, 40)

## -----------------------------------------------------------------------------
morty = function(a){
  (40.4*(exp(-5*a)*.8 + exp(-0.5*a)*.15 + exp(-.07*a)*.05) +  0.3*(a-10) + .1)/365/1000/80
}

## ----fig.height=4, fig.width=7------------------------------------------------
aa = seq(0, 80, by=.1)
plot(aa, morty(aa)*365000,  type = "l",  xlab = "Age (in Years)", ylab = expression(list(mu(a), "Death Rate")))

## -----------------------------------------------------------------------------
library(ramp.xds)
library(ramp.func)
library(ramp.forcing)
library(ramp.demog)

## -----------------------------------------------------------------------------
mod <- xds_setup_eir(eir=1/365)
mod <- setup_deaths("func", mod, list(F_mu=morty))
mod <- xds_solve_cohort(mod, Amax=80*365,da=10)

## ----fig.height=4, fig.width=7------------------------------------------------
plot(mod$outputs$time/365, mod$outputs$orbits$XH[[1]]$H, type = "l", xlab = "Age(in Days)", ylab = "H")

## -----------------------------------------------------------------------------
tail(mod$outputs$orbits$XH[[1]]$H)

