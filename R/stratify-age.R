
#' @title Segment by age
#'
#' @description This function adds 
#' adds age structure to a model. The
#' user passes an age mesh, and optional
#' arguments to set search weights by age 
#' and mortality by age. The model segments
#' each one of the \eqn{N_h} strata into
#' \eqn{N_a} compartments, creating a 
#' new model with \eqn{N_h \times N_a} strata. 
#' 
#' This function uses the break points in `mesh` to split the population:
#' + replicate the parameters \eqn{N_a} times
#' + replicate the initial conditions for disease states \eqn{N_a} times
#' + split the human population density \eqn{(H)} using `mu_a`
#' + replicate and modify the search weights by age using `search_by_age` 
#' + reshape the time spent matrix and time away
#' + set up the deaths matrix
#' + modify births function so that all births occur in the newborn state for the youngest age class
#' + re-index the dependent variables  
#'  
#' @param mesh a mesh on age
#' @param xds_obj an **`xds`** model object
#' @param search_by_age search weights by age
#' @param F_mu mortality rates function
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @export
stratify_by_age = function(mesh, xds_obj, search_by_age=1, F_mu=makepar_F_exp(1/80/365), i=1){

  Na <- length(mesh)+1
  search_by_age = checkIt(search_by_age, Na)
  mu_a = mortality_age_to_stage(mesh, Fmu) 
  
  xds_obj <- add_age_to_strata_obj(xds_obj, i)
  xds_obj <- update_strata_by_membership(xds_obj, i) 

  xds_obj <- rep_XH_inits_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_XH_pars_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_time_away_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_travel_eir_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_death_rates_by_strata(Nw, xds_obj, i)
  xds_obj <- rep_blood_search_weights_by_strata(Nw, xds_obj, i)
  xds_obj <- stretch_timespent(Nw, xds_obj, i)
  xds_obj <- update_birth_rates_by_age(membership, xds_obj, i)
  xds_obj <- split_H_by_age(mu_a, xds_obj, i)
  xds_obj <- make_indices(xds_obj)
    
  return(xds_obj)
}

#' @title Add age to strata obj 
#'
#' @description The `strata` object supports
#' progressive stratification. 
#' 
#' @param mesh a mesh on age
#' @param mu_a survival by age
#' @param xds_obj an **`xds`** model object
#' @param i the species index
#'
#' @returns an **`xds`** model object
#' @keywords internal
#' @export
add_age_to_strata_obj = function(mesh, mu_a, xds_obj, i=1){
  
  Na <- length(mesh)+1
  stopifnot(length(mu_a) == Na)
  xds_obj <- setup_strata(xds_obj, i)
  strata <- get_strata(xds_obj, i)
  aging = list()
  aging$N = Na
  aging$matrix = UnevenAgingMatrix(mu_a, mesh)
  aging$mu_a = mu_a 
  aging$mesh = mesh
  strata$aging = aging
  xds_obj$XH_obj[[i]]$strata = strata
  
  xds_obj$nStrata[i] = xds_obj$nStrata[i]*Na
  return(xds_obj)
}




#' @title Change relative biting by age
#'
#' @description
#' Change the function for relative biting rates by age
#'
#' @param Xname the module name 
#' @param mesh an age mesh to evaluate
#' @param eir the mean EIR
#' @param age_par a function object
#' @param season_par a function object
#' @param trend_par a function object
#' @param shock_par a function object
#' 
#'
#' @return an **`xds`** object
#' 
#' @importFrom ramp.func make_function
#' 
#' @keywords internal
#' @export
evaluate_age_mesh = function(Xname, mesh, eir,
                             age_par = makepar_F_c(1), 
                             season_par = makepar_F_c(1), 
                             trend_par = makepar_F_c(1), 
                             shock_par = makepar_F_c(1)){
  
  cohort_model <- xds_setup_eir(Xname=Xname, eir=eir, HPop=1000,
                                season_par=season_par, trend_par=trend_par, 
                                age_par=age_par, shock_par=shock_par)
  cohort_model <- rebuild_forcing_functions(cohort_model)
  age_structure_model <- stratify_by_age(mesh, cohort_model)
  cohort_model <- xds_solve_cohort(cohort_model, Amax = 2*max(mesh), da=1)
  age_structure_model <- xds_solve_eir(age_structure_model, Tmax = 2*max(mesh), dt=1)
  
  return(xds_obj)
}

#' @title Create a cohort matrix
#' @description Return a matrix `dH`
#' @param deathrates the death rates for
#' @param ageMesh a partition on human age
#' @return the steady states as a named vector
#' @export
UnevenAgingMatrix = function(deathrates, ageMesh){
  N = length(ageMesh)
  checkIt(deathrates, N+1)
  ageUp = c(1/ageMesh,0)
  Aging = diag(-ageUp-deathrates)
  Aging[cbind(1+1:N, 1:N)] = 1/ageMesh
  return(Aging)
}
