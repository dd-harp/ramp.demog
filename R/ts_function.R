


#' Make a Composed Time Series Function
#'
#' @description
#' Build a function that returns
#' a product of four configurable elements:
#' + \eqn{\bar x}: a mean value
#' + \eqn{F_S(t)}: a seasonal pattern
#' + \eqn{F_T(t)}: a trend
#' + \eqn{F_K(t)}: a shock
#'
#' The return value is a function of the form:
#' + \eqn{F_t(t)} if `form == "t"`
#' + \eqn{F_t(t, V(t))} if `form == "tV"`
#'
#' The user can pass any vector `times` and the interval is
#' set to
#' + \eqn{t_0 = \mbox{min(times)}}, and
#' + \eqn{t_1 = \mbox{max(times)}}.
#'
#' A normalizing constant is set such that the daily average over
#' the interval \eqn{(t_0, t_1)} is \eqn{\bar x}:
#' \deqn{\int_{t_0}^{t_1} F_t(t) dt = \bar x \left(t_1-t_0\right).}
#'
#' The component functions are specified by
#' passing `F_obj` objects (see [F_obj]):
#' + `season_par` creates \eqn{F_S(t)} or `F_season` (*eg,* using [makepar_F_sin])
#' + `trend_par` creates \eqn{F_T(t)} or `F_trend` (*eg,* using [makepar_F_spline])
#' + `shock_par` creates \eqn{F_K(t)} or `F_shock` (*eg,* using [makepar_F_sharkbite])
#'
#' @param avg the average
#' @param season_par seasonality function for
#' @param trend_par trend function parameters
#' @param shock_par trend function parameters
#' @param N the length of the return value
#' @param times normalize \eqn{t_0 = \mbox{min(times)}} to \eqn{t_1 = \mbox{max(times)}}
#' @param norm_with_shocks if FALSE, set {\eqn{F_K(t)=1}} for normalization
#' @param form functional form: "t" returns \eqn{F(t)}; "tV" returns \eqn{F(t,V)}
#' @param options configurable options
#'
#' @return a function
#' @export
make_tsy_function = function(avg=1,
                            season_par = list(),
                            trend_par = list(),
                            shock_par = list(),
                            times = c(0,365),
                            norm_with_shocks=FALSE,
                            N=1,
                            form = "tV",
                            options=list()){
  with(options,{
    t0=min(times); t1=max(times)
    avg = check_length(avg, N)

    if(is.numeric(trend_par)){
      yy <- trend_par
      tt <- seq(0, A, length.out=length(yy))
      trend_par <- makepar_F_spline(tt, yy, 2)
    }

    F_season = make_F_t(season_par)
    F_trend = make_F_t(trend_par)
    F_shock = make_F_t(shock_par)

    if(norm_with_shocks) F_K = F_shock
    if(!norm_with_shocks) F_K = make_F_t(makepar_F_c(1))

    Ft = function(t){
      F_season(t)*F_trend(t)*F_K(t)
    }

    normit = (t1-t0)/integrate(Ft, t0, t1)$value

    return(function(t, y, xds_obj, s=1){avg*normit*F_season(t)*F_trend(t)*F_shock(t)})
  })}
