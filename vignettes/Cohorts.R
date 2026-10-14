## -----------------------------------------------------------------------------
devtools::load_all()

## -----------------------------------------------------------------------------
mod <- xds_setup_eir(eir=1/365)
mesh = seq(0, 1825, length.out=20)[-1]
mod <- xds_solve_cohort(mod)
pr_age <- get_pr_by_age_mesh(mesh, mod)
with(pr_age, plot(age, pr, type = "h", xlim = range(0,mesh), ylim=range(0, pr)))
with(pr_age, lines(lb, pr, type = "s"))

