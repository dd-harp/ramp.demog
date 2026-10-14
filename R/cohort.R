

#' @title Cohort PR
#'
#' @description Call [xds_solve_cohort] with a given birthday 
#' and average eir, and call [get_pr_by_age_mesh], 
#' which computes the average PR 
#' for the breakpoints defined in an age mesh. 
#' 
#' 
#' @param mesh a mesh on age
#' @param xds_obj an **`xds`** model object
#' @param bday the birthday for a cohort
#' @param eir the average eir 
#'
#' @returns the average PR 
#' 
#' @export
xds_cohort_pr = function(mesh, xds_obj, bday=0, eir=c()){
  if(length(eir)>0) xds_obj$EIR_obj$eir <- eir
  xds_obj = xds_solve_cohort(xds_obj, birthday=bday, Amax = max(mesh), da=1)
  return(get_pr_by_age_mesh(mesh, xds_obj))
}

#' @title Get cohort pr by age
#'
#' @description For cohorts, get the pr for 
#' age bins defined by `mesh`. 
#' 
#'
#' 
#' @param mesh a mesh on age
#' @param xds_obj an **`xds`** model object
#' @param method the diagnostic method, for [ramp.xds::get_PR]
#' @param i the species index
#'
#' @returns the average PR 
#' 
#' @export
get_pr_by_age_mesh = function(mesh, xds_obj, method = "true", i=1){
  if(with(get_XH_orbits(xds_obj, i=1), !exists("true_pr"))) 
    xds_obj = xds_solve_cohort(xds_obj, Amax = max(mesh), da=1)
  pr_and_age <- get_PR(xds_obj)
  age <- pr_and_age$time
  if(max(age)< max(mesh)){
    xds_obj = xds_solve_cohort(xds_obj, Amax = max(mesh), da=1)
    pr_and_age <- get_PR(xds_obj)
    age <- pr_and_age$time
  } 
  pr <- pr_and_age$pr
  H <- get_XH_orbits(xds_obj)$H
  
  PR = A = 0*mesh
  ix = which(age < mesh[1])
  PR[1] = mean(pr[ix], weights=H[ix]) 
  A[1] = mean(age[ix], weights=H[ix])
  for(i in 2:length(mesh)){
    ix = which(mesh[i-1] <= age & age < mesh[i])
    PR[i] = mean(pr[ix], weights=H[ix]) 
    A[i] = mean(age[ix], weights=H[ix])
  }
  N=length(mesh)
  lb = c(0,mesh[-N])
  mid = (lb+mesh)/2
  return(list(age=A, pr=PR, lb=c(0,mesh[-N]), mid=mid, mesh=mesh))
}