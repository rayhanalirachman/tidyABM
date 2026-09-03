# 11. Daisyworld  (Daisyworld, NetLogo Models Library, after Watson & Lovelock 1983)
#
# Patches only, and it needs nothing past the lattice constructor -- which is
# the useful proof that "patch-only" is not "simple". A two-way global/lattice
# feedback loop, age structure and neighbour-seeded reproduction all fall out
# of steps that already exist: seeding an empty patch from a random daisy
# neighbour is `abm_match(pair = "network")`.
#
# Note the two `abm_rules()` calls for albedo and local_t. Every rule in one
# call sees the state at the START of the step, so a rule cannot read a column
# another rule in the same call just wrote.
#
# Reported: the Gaia result -- equilibrium temperature stays far flatter than
# the bare physics would give, held there by the shifting black/white mix.

suppressPackageStartupMessages(source("_setup.R", chdir = TRUE))

run_one <- function(lum, w = 40, ticks = 300, seed = 1) {
  d <- abm_setup(
    agents = abm_agents(
      kind = ~sample(c("white", "black", "none"), n, TRUE, c(0.2, 0.2, 0.6)),
      age  = 0L),
    network = abm_network(type = "grid", dims = c(w, w),
                          diagonals = TRUE, torus = TRUE),
    globals = list(luminosity = lum, global_temp = 0), seed = seed)

  go <- abm_go(
    abm_rules(albedo ~ case_when(kind == "white" ~ 0.75,
                                 kind == "black" ~ 0.25,
                                 TRUE            ~ 0.40), .scope = "population"),
    abm_rules(local_t ~ luminosity * (1 - albedo) * 40 - 5, .scope = "population"),
    abm_neighbours(t_in ~ mean(local_t)),
    abm_rules(temperature ~ 0.5 * local_t + 0.5 * t_in),

    abm_rules(age  ~ if_else(kind != "none", age + 1L, 0L),
              kind ~ if_else(age > 25L, "none", kind), .scope = "population"),

    abm_match(pair = "network", eligible = kind == "none"),
    abm_rules(kind ~ if_else(
      !is.na(partner_kind) & partner_kind != "none" &
        runif(n()) < pmax(0, 1 - ((temperature - 22.5) / 17.5)^2),
      partner_kind, kind)),

    abm_global(global_temp ~ mean(temperature),
               white_f ~ mean(kind == "white"),
               black_f ~ mean(kind == "black"),
               bare_t  ~ mean(luminosity * (1 - 0.40) * 40 - 5)))

  g <- abm_globals(abm_run(d, go, ticks = ticks, seed = seed,
                           record = "globals", progress = FALSE))
  tail(g, 1)
}

cat("40x40 torus, 300 ticks, luminosity swept\n\n")
lums <- seq(0.7, 1.4, by = 0.1)
tab <- do.call(rbind, lapply(lums, function(l) {
  o <- run_one(l)
  data.frame(luminosity = l,
             temp = round(o$global_temp, 2),
             bare_rock = round(o$bare_t, 2),
             white = round(o$white_f, 2),
             black = round(o$black_f, 2))
}))
print(tab, row.names = FALSE)
cat("\n  `bare_rock` is what the temperature would be with no daisies at all.\n")
cat("  Across the middle of the range the daisy mix shifts from black-heavy\n")
cat("  to white-heavy and holds `temp` far flatter than `bare_rock` moves.\n")
