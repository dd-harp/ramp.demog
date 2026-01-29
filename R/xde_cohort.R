
#' @title set phase
#'
#' @description
#' Set the phase for the eir seasonal pattern for
#' a `cohort` model
#' and return the **`ramp.xds`** model object
#'
#' @inheritParams set_season_phase
#'
#' @export
set_season_phase.cohort = function(phase, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$EIRpar$season_par$phase) == length(phase))
  xds_obj$EIRpar$season_par$phase = phase
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}

#' @title set bottom
#'
#' @description
#' Set the bottom for the eir seasonal pattern for
#' a `cohort` model
#' and return the **`ramp.xds`** model object
#'
#' @inheritParams set_season_bottom
#'
#' @export
set_season_bottom.cohort = function(bottom, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$EIRpar$season_par$bottom) == length(bottom))
  xds_obj$EIRpar$season_par$bottom = bottom
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}

#' @title Set pw, a seasonality shape parameter
#'
#' @description
#' Set the pw for the eir seasonal pattern for
#' a `cohort` model
#' and return the **`ramp.xds`** model object
#'
#' @inheritParams set_season_pw
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
set_season_pw.cohort = function(pw, xds_obj, s=1, compile_F=TRUE){
  stopifnot(length(xds_obj$EIRpar$season_par$pw) == length(pw))
  xds_obj$EIRpar$season_par$pw = pw
  if(compile_F == TRUE) xds_obj = update_F_season(xds_obj, s)
  return(xds_obj)
}

#' @title Update the seasonality function
#'
#' @description Update `F_season`
#'
#' @inheritParams update_F_season
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
update_F_season.cohort = function(xds_obj, s=1){
  xds_obj$EIRpar$F_season <- make_function(xds_obj$EIRpar$season_par)
  return(xds_obj)
}

#' @title Build a Model of Human / Host Cohort Dynamics
#' @description
#' \loadmathjax
#'
#' A modified version of [xds_setup] to setup up studies of cohort
#' dynamics.
#'
#' The **`xds`** object defines `frame = class(frame) = 'cohort'` but there
#' is no `cohort` case for [xds_solve]. Instead, cohort
#' dynamics are studied using [xds_solve_cohort], which was designed
#' to compare the outcomes for cohorts of different ages when exposure is
#' changing.
#'
#' The interface includes options to configure a function
#' describing `F_eir` as a function of time, with seasonal components
#' and a trend. Exposure in a cohort is a function of its age, including
#' a function that modifies exposure by age.
#'
#' @seealso [xds_setup] and [xds_setup_human] and [xds_solve_cohort]
#'
#' @param eir is the entomological inoculation rate
#' @param F_season a function describing a seasonal pattern over time
#' @param season_par parameters to configure a seasonality function using [make_function]
#' @param F_trend a function describing a temporal trend over time
#' @param trend_par parameters to configure a trends function using [make_function]
#' @param F_age a assigning a biting weight by age
#' @param age_par parameters to configure an age weights function using [make_function]
#' @param xds is `ode` or `dde` or `dts` for ordinary OR delay differential OR difference equations
#' @param Xname is a character string specifying an **X** Component module
#' @param XHoptions a list to configure the **X** Component module
#' @param HPop is the number of humans in each stratum
#' @param searchB is a vector of search weights for blood feeding
#' @param model_name is a name for the model (arbitrary)
#' @return an **`xds`** object
#' @export
xds_setup_cohort = function(eir=1,
                            F_season = F_flat, season_par = list(),
                            F_trend = F_flat, trend_par = list(),
                            F_age = F_flat, age_par = list(),
                            xds = 'ode',

                            # Dynamical Components
                            Xname = "SIS",
                            XHoptions = list(),

                            # Model Structure
                            HPop=1000,
                            searchB = 1,

                            # Human Strata / Options
                            model_name = "unnamed"
){
  nPatches = length(HPop)
  residence = rep(1, length(HPop))
  membership = 1
  xds_obj <- make_xds_object_template(xds, 'cohort', nPatches, membership, residence)

  xds_obj$EIRpar <- list()
  xds_obj$EIRpar$eir <- eir
  xds_obj$EIRpar$scale <- 1

  xds_obj$EIRpar$F_season <- F_season
  xds_obj$EIRpar$season_par <- season_par
  if(length(season_par)>0){
    xds_obj$EIRpar$F_season <- make_function(season_par)
  }

  xds_obj$EIRpar$F_trend <- F_trend
  xds_obj$EIRpar$trend_par <- trend_par
  if(length(trend_par)>0){
    xds_obj$EIRpar$F_trend <- make_function(trend_par)
  }

  xds_obj$EIRpar$F_age <- F_age
  xds_obj$EIRpar$age_par <- age_par
  if(length(age_par)>0){
    xds_obj$EIRpar$F_age <- make_function(age_par)
  }

  # Aquatic Mosquito Dynamics
  xds_obj       <- setup_L_obj("trivial", xds_obj, 1, list())
  xds_obj       <- setup_L_inits(xds_obj, 1)

  # Adult Mosquito Dynamics
  xds_obj           <- setup_MY_obj("trivial", xds_obj, 1, list())

  # Human Dynamics
  xds_obj$Xname <- Xname
  xds_obj       <- setup_XH_obj(Xname, xds_obj,  1, XHoptions)
  xds_obj       <- setup_XH_inits(xds_obj, HPop, 1, XHoptions)

  xds_obj = make_indices(xds_obj)

  wts        <- checkIt(searchB, xds_obj$nStrata)
  xds_obj       <- change_blood_search_weights(wts, xds_obj, 1, 1)

  # Probably Not Necessary
  y0 <- as.vector(unlist(get_inits(xds_obj)))
  xds_obj <- BloodFeeding(0, y0, xds_obj)

  xds_obj$model_name <- model_name

  return(xds_obj)
}

#' @title Cohort dynamics for a human / host model
#' @description
#' Compute the states for a model \eqn{\cal X} in a cohort of humans / hosts
#' as it ages, up to age \eqn{A} years of age
#' @details
#' This method substitutes age for time: a model
#' \deqn{\cal X(t)}
#' is solved with respect to age \eqn{a}:
#' \deqn{\cal X(a),}
#' where the daily EIR is computed by a *trace* function with four elements:
#' + \eqn{\bar E} or `eir`, the mean daily EIR,
#' + \eqn{\omega(a)} or `F_age,` a function of age
#' + \eqn{S(t)} or `F_season,` a function of time of year
#' + \eqn{T(t)} or `F_trend,` a function describing a trend
#'
#' For a cohort born on day \eqn{B},
#' the function creates a mesh on age / time, where time and age
#' are related by the formula:
#' \deqn{t = B + a}
#' and the trace function is:
#'  \deqn{E(a, t) = \hat E \; \omega(a) \; S(t)\; T(t) }
#' The output is returned as `xds_obj$outputs$cohort`
#' @param xds_obj an **`xds`** model object
#' @param bday the cohort birthday
#' @param A the maximum age to compute (in years)
#' @param da the output interval (age, in days)
#' @return an **`xds`** object
#' @export
xds_solve_cohort = function(xds_obj, bday=0, A=10, da=10){

  age <- seq(0, A*365, by=da)

  y0 = get_inits(xds_obj, flatten=TRUE)

  xde_cohort_desolve(bday, y0, age, xds_obj) -> deout
  de_vars <- deout[,-1]

  xds_obj$outputs$orbits <- list()
  xds_obj$outputs$orbits$XH <- list()
  xds_obj$outputs$orbits$XH[[1]] <- parse_orbits(de_vars, xds_obj)$XH[[1]]
  xds_obj$outputs$last_y <- tail(de_vars, 1)
  xds_obj$outputs$orbits$age <- age
  xds_obj$outputs$orbits$time <- age+bday
  tm <- age + bday
  xds_obj$outputs$time <- tm
  xds_obj$outputs$terms <- list()
  xds_obj$outputs$terms$EIR <- list()
  xds_obj$outputs$terms$EIR[[1]] <- with(xds_obj$EIRpar, eir*F_season(tm)*F_trend(tm)*F_age(age))
  return(xds_obj)
}

#' @title Differential equation models for human cohorts
#' @description Compute derivatives for [deSolve::ode] or [deSolve::dede] using
#' generic methods for each model component.
#' @param age host age
#' @param y the state variables
#' @param xds_obj an **`xds`** model object
#' @param birthday the cohort birthday
#' @return a [list] containing the vector of all state derivatives
#' @export
xde_cohort_derivatives <- function(age, y, xds_obj, birthday) {

  t = age+birthday

  # EIR: entomological inoculation rate trace
  xds_obj$EIR[[1]] <- with(xds_obj$EIRpar, eir*F_trend(t)*F_season(t)*F_age(age))

  # FoI: force of infection
  xds_obj <- Exposure(t, y, xds_obj)

  # state derivatives
  dXH <- dXHdt(age, y, xds_obj, 1)

  return(list(c(dXH)))
}

#' @title Solve a system of equations as an ode
#' @description Implements for ordinary differential equations
#' @param birthday a cohort birthday
#' @param inits initial values
#' @param times = the times
#' @param xds_obj an **`xds`** model object
#'
#' @return a [list]
#' @export
xde_cohort_desolve  = function(birthday, inits, times, xds_obj){
  UseMethod("xde_cohort_desolve", xds_obj$xde)
}

#' @title Solve a system of equations as a dde
#' @description Implements for delay differential equations
#' @inheritParams xde_cohort_desolve
#'@return a [list]
#' @export
xde_cohort_desolve.dde = function(birthday, inits, times, xds_obj){
  return(deSolve::dede(y=inits, times=times, func=xde_cohort_derivatives, parms=xds_obj,
                       method = "lsoda", birthday=birthday))
}

#' @title Solve a system of equations as a ode
#' @description Implements for delay differential equations
#' @inheritParams xde_cohort_desolve
#'@return a [list]
#' @export
xde_cohort_desolve.ode = function(birthday, inits, times, xds_obj){
  return(deSolve::ode(y=inits, times=times, func=xde_cohort_derivatives, parms = xds_obj,
                      method = "lsoda", birthday=birthday))
}


#' @title set mean forcing
#'
#' @description
#' Set the mean daily EIR for a `cohort` model
#'
#' @inheritParams set_mean_forcing
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
set_mean_forcing.cohort = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$EIRpar$eir) == length(X))
  xds_obj$EIRpar$eir = X
  return(xds_obj)
}


#' Shrink an xds xds_obj object
#'
#' @param xds_obj an **`xds`** model object
#'
#' @returns a smaller xds xds_obj object
#' @export
xds_shrink.cohort = function(xds_obj){
  xds_obj$F_eir <- list()
  xds_obj$EIRpar$F_season <- list()
  xds_obj$EIRpar$F_trend <- list()
  xds_obj$EIRpar$F_age <- list()
  xds_obj$outputs <- list()
  return(xds_obj)
}


#' @title set yy
#'
#' @description
#' Set the yy for the eir seasonal pattern for
#' a `cohort` model
#' and return the **`ramp.xds`** model object
#'
#' @inheritParams set_spline_y
#'
#' @export
set_spline_y.cohort = function(X, xds_obj, s=1){
  stopifnot(length(xds_obj$EIRpar$trend_par$yy) == length(X))
  xds_obj$EIRpar$trend_par$yy = X
  return(xds_obj)
}


#' @title Update the trend function
#'
#' @description Update `F_trend`
#'
#' @param xds_obj an **`xds`** model object
#' @param s the vector species index
#'
#' @return a **`ramp.xds`** model object
#'
#' @export
update_F_trend.cohort = function(xds_obj, s=1){
  xds_obj$EIRpar$F_trend <- make_function(xds_obj$EIRpar$trend_par)
  return(xds_obj)
}


#' @title Difference equation models for human cohorts
#' @description Compute and update the state variables for
#' a cohort
#' @inheritParams dts_update
#' @return a [vector] containing the vector of all state derivatives
#' @export
dts_update.cohort <- function(t, y, xds_obj) {

  xds_obj <- xds_compute_terms(t, y, xds_obj)

  # state derivatives
  XHt <- dts_update_XHt(t, y, xds_obj, 1)
  if(xds_obj$nHosts > 1)
    for(i in 2:xds_obj$nHosts)
      XHt <- c(XHt, dts_update_XHt(t, y, xds_obj, i))

  return(c(XHt))
}
