
#' @title Check migration 
#' 
#' @description
#' Check that 
#' + the migration matrix is numeric
#' + the migration matrix is not negative
#' + the migration matrix length is the number of strata
#' 
#' @param migration_matrix the migration matrix
#' @param Nh the number of population strata
#' 
#' @export
check_migration = function(migration_matrix, Nh){
  stopifnot(is.numeric(migration_matrix))
  stopifnot(migration_matrix>=0)
  stopifnot(length(migration_matrix)==Nh) 
}

#' @title Change the migration matrix
#' 
#' @description
#' Modify the migration matrix for a model with static population 
#' rates. 
#' 
#' 
#' @param migration_matrix migration matrix 
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
change_migration = function(migration_matrix, xds_obj, i=1){
  check_migration(migration_matrix, xds_obj$nStrata[i])
  xds_obj$XH_obj[[i]]$strata$migration$matrix <- migration
  xds_obj <- check_dHdt(xds_obj, i)
  return(xds_obj)
}

#' @title Set up migration 
#'
#' @param name a  or setup function name
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#' @param options configuration options
#'
#' @return an **`xds`** object
#'
#' @export
setup_migration = function(name, xds_obj, options = list(), i=1){
  if(is.numeric(name)) class(options) = "as_is"
  if(is.character(name)) class(options) = name
  UseMethod("setup_migration", options)
}

#' @title Set up a migration 
#'
#' @description If an options list is passed
#' as the first argument, then set 
#' + `migration_matrixname = name$name` 
#' + `options = name`
#' and call `setup_migration(migration_matrixname, xds_obj, options, s)` 
#'
#' @inheritParams setup_migration
#'
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_migration.list = function(name, xds_obj, options=list(), i=1){
  options = name
  migration_name = name$name
  if(is.null(migration_name)) migration_name = "matrix"
  xds_obj <- setup_migration(migration_name, xds_obj, options, i)
  return(xds_obj)
}

#' @title Set up no migration  
#' @description Don't change anything 
#' @inheritParams setup_migration
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_migration.no_setup = function(name, xds_obj, options = list(), i=1){
  return(xds_obj)
}

#' @title Set up no migration  
#' @description Don't change anything 
#' @inheritParams setup_migration
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_migration.zero = function(name, xds_obj, options = list(), i=1){
  xds_obj$XH_obj[[i]]$strata$migration$migration_matrix = rep(0, xds_obj$nStrata[i])
  class(xds_obj$XH_obj[[i]]$strata$migration) = "zero" 
  return(xds_obj)
}


#' @title Set up migration matrix
#' 
#' @description 
#' Pass pre-configured constant migration matrix. If it passes 
#' the checks, it replaces the current migration matrix.
#' 
#' If called with `name = "as_is"`, the migration
#' must be at `options$migration` 
#' 
#' @inheritParams setup_migration
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_migration.matrix = function(name, xds_obj, options=list(), i=1){
  migration_matrix <- options$migration_matrix
  xds_obj <- change_migration(migration_matrix, xds_obj, i)
  return(xds_obj)
}

#' @title Set up a migration 
#' @description 
#' Pass a pre-configured migration . If it passes 
#' the checks, it replaces the current migration .
#' 
#' If called with `name = "as_is"`, the migration
#'  must be at `options$migration` 
#' 
#' @inheritParams setup_migration
#' @return an **`xds`** object
#' @keywords internal
#' @export
setup_migration.as_is = function(name, xds_obj, options=list(), i=1){
  
  if(is.list(options)) 
    migration_matrix = options$migration_matrix
  if(is.numeric(name))
    migration_matrix = name
  
  xds_obj <- change_migration(migration_matrix, xds_obj, i)
  
  return(xds_obj)
}

#' @title Get the migration 
#'
#' @param xds_obj an **`xds`** model object
#' @param i the host species index
#'
#' @return a []
#' @export
get_migration_matrix = function(xds_obj, i=1){
  return(xds_obj$XH_obj[[i]]$strata$migration$migration_matrix)
}

