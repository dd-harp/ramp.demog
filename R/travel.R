
#' @title Setup the Travel Sub-Object
#'
#' @description Setup the port sub-object to handle
#' travel dynamics. Initialises `travel_obj` on the `XY_interface`
#' and sets static (zero) defaults for `F_travel` and `F_travel_eir`.
#'
#' @note The parameters `time_at_home` and `travel_EIR` are
#' initialised by [ramp.xds::setup_importation]; this function
#' only sets up the port machinery.
#'
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_travel_object = function(xds_obj){
  xds_obj$XY_interface$travel_obj <- list()
  class(xds_obj$XY_interface$travel_obj) <- "setup"
  xds_obj$XY_interface$travel_obj[[1]] <- list()
  xds_obj <- setup_F_travel("static", xds_obj, 1)
  xds_obj <- setup_F_travel_eir("static", xds_obj, 1)
  return(xds_obj)
}

#' @title Travel
#'
#' @description Dispatches on `class(xds_obj$XY_interface$travel_obj)`
#' to update time at home and travel EIR.
#'
#' @param t current simulation time
#' @param y variables
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
Travel <- function(t, y, xds_obj) {
  UseMethod("Travel", xds_obj$XY_interface$travel_obj)
}

#' @title Travel (static)
#'
#' @description For a static travel model, the function
#' does not update anything.
#'
#' @inheritParams Travel
#' @return an **`xds`** object
#' @keywords internal
#' @export
Travel.static <- function(t, y, xds_obj) {
  return(xds_obj)
}

#' @title Travel (setup)
#'
#' @description Resets the `travel_obj` class to `static`, triggers
#' a setup update to the blood feeding model, and then calls
#' [travel_dynamics] once.
#'
#' @inheritParams Travel
#' @return an **`xds`** object
#' @keywords internal
#' @export
Travel.setup <- function(t, y, xds_obj) {
  class(xds_obj$XY_interface$travel_obj) <- "static"
  xds_obj$XY_interface <- trigger_setup(xds_obj$XY_interface)
  return(xds_obj)
}

#' @title Travel (dynamic)
#'
#' @description Dynamically updates time at home and travel EIR.
#'
#' @inheritParams Travel
#' @return an **`xds`** object
#' @keywords internal
#' @export
Travel.dynamic <- function(t, y, xds_obj) {
  return(travel_dynamics(t, y, xds_obj))
}

#' @title Travel Dynamics
#'
#' @description Computes and updates `time_at_home` and `travel_EIR`
#' on the `XY_interface` and `terms`, respectively.
#'
#' @param t current time
#' @param y the state variable vector
#' @param xds_obj an **`xds`** model object
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
travel_dynamics <- function(t, y, xds_obj){
  for(s in 1:xds_obj$nHostSpecies){
    F_travel <- xds_obj$XY_interface$travel_obj[[s]]$F_travel
    V <- get_variables(t, y, F_travel, xds_obj)
    xds_obj$XY_interface$time_at_home[[s]] <- 1 - F_travel(t, V)

    F_travel_eir <- xds_obj$XY_interface$travel_obj[[s]]$F_travel_eir
    V <- get_variables(t, y, F_travel_eir, xds_obj)
    xds_obj$terms$travel_EIR[[s]] <- F_travel_eir(t, V)
  }
  return(xds_obj)
}

#' @title Set up the travel model
#'
#' @description Setup a model for time spent traveling
#' and exposure while traveling.
#'
#' @param setup_name a string to dispatch `setup_F_travel`
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options setup options for `F_travel`
#'
#' @return an **`xds`** object
#' @export
#' @keywords internal
setup_F_travel = function(setup_name, xds_obj, i, options=list()){
  class(setup_name) = setup_name
  UseMethod("setup_F_travel", setup_name)
}

#' @title Set up no travel (static)
#'
#' @description Setup a static (zero) model for time spent traveling.
#'
#' @inheritParams setup_F_travel
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_travel.static = function(setup_name, xds_obj, i, options=list()){
  F_travel <- F_zero
  class(F_travel) = "list"
  xds_obj$XY_interface$travel_obj[[i]]$F_travel = F_travel
  return(xds_obj)
}

#' @title Set up travel as a time series function
#'
#' @description Setup a time series function for time spent traveling.
#'
#' @inheritParams setup_F_travel
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_travel.ts_func = function(setup_name, xds_obj, i, options=list()){
  class(xds_obj$XY_interface$travel_obj) = 'dynamic'
  class(xds_obj$XY_interface) = 'dynamic'
  class(xds_obj$beta) = 'dynamic'
  xds_obj$XY_interface$travel_obj[[i]]$F_travel = make_ts_function(options)
  return(xds_obj)
}

#' @title Setup the Travel EIR model
#'
#' @description Setup a model for the dEIR experienced while traveling.
#'
#' @param setup_name a string to dispatch `setup_F_travel_eir`
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options a named list to set up `F_travel_eir`
#'
#' @return an **`xds`** object
#' @export
#' @keywords internal
setup_F_travel_eir = function(setup_name, xds_obj, i, options=list()){
  class(setup_name) = setup_name
  UseMethod("setup_F_travel_eir", setup_name)
}

#' @title Set up a static (zero) travel EIR
#'
#' @description Setup a static zero model for exposure while traveling.
#'
#' @inheritParams setup_F_travel_eir
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_travel_eir.static = function(setup_name, xds_obj, i, options=list()){
  F_travel_eir <- F_zero
  class(F_travel_eir) = "list"
  xds_obj$XY_interface$travel_obj[[i]]$F_travel_eir = F_travel_eir
  return(xds_obj)
}

#' @title Set up travel EIR as a time series function
#'
#' @description Setup a time series function for exposure while traveling.
#'
#' @inheritParams setup_F_travel_eir
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_F_travel_eir.ts_func = function(setup_name, xds_obj, i, options=list()){
  xds_obj$XY_interface$travel_obj[[i]]$F_travel_eir = make_ts_function(options)
  return(xds_obj)
}
