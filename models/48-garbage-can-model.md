# 48. A garbage can model of organizational choice (Cohen, March & Olsen 1972)

**Concept**

- Setup: three populations that meet only by accident, 10 choice
  opportunities, 20 problems and 10 decision makers, each entering over time,
  with an access structure saying which problems may reach which choices
- Go: every live problem attaches itself to the accessible choice closest to
  being made; every decision maker puts its energy where that energy makes the
  most difference; a choice whose accumulated energy covers what is attached to
  it is made
- Output: how the decisions get made, by *resolution*, by *flight* or by
  *oversight*, and how many problems are actually solved

**Package**

```r
n_choices <- 10; n_problems <- 20; n_dms <- 10
n_kinds <- 3; need <- 1; power <- 0.1; base <- 1

m <- abm_setup(
  agents = list(
    choices  = abm_agents(n = n_choices, entry = ~seq_len(n), energy = 0, added = 0,
                          kind = ~rep(seq_len(n_kinds), length.out = n),
                          n_att = 0L, ever = 0L, deficit = need,
                          made = FALSE, fresh = FALSE,
                          members = ~vector("list", n),
                          style = NA_character_, made_at = NA_integer_),
    problems = abm_agents(n = n_problems,
                          entry = ~rep(seq_len(n_choices), length.out = n),
                          kind  = ~sample(seq_len(n_kinds), n, replace = TRUE),
                          solved = FALSE, att = NA_integer_),
    dms      = abm_agents(n = n_dms, power = power,
                          kind = ~rep(seq_len(n_kinds), length.out = n))),
  globals = list(now = 0),
  seed    = 1)

go <- abm_go(
  abm_global(now ~ now + 1),
  abm_rules(n_att ~ 0L, added ~ 0, fresh ~ FALSE,
            members ~ vector("list", n()), .scope = "population"),

  # every live problem attaches to the accessible choice that is closest to
  # being made: the thing being minimised is an energy deficit, not a distance
  abm_match(pair = "nearest",
            cost = if_else(open | kind == own_kind, deficit, NA_real_),
            eligible = !!live_problem, among = !!live_choice),
  abm_tell(n_att ~ 1L, to = .partner, .resolve = "sum",
           when = !!live_problem & !is.na(.partner)),
  # ...and tells the choice who it is, so the choice can answer back later
  abm_tell(members ~ .id, to = .partner, .resolve = "collect",
           when = !!live_problem & !is.na(.partner)),

  abm_match(pair = "nearest",
            cost = if_else(open | kind == own_kind, deficit, NA_real_),
            eligible = .group == "dms", among = !!live_choice),
  abm_tell(added ~ power, to = .partner, .resolve = "sum",
           when = .group == "dms" & !is.na(.partner)),
  abm_rules(energy ~ energy + added, ever ~ ever + n_att, .scope = "population"),

  abm_rules(fresh ~ !made & energy >= need * (base + n_att), .scope = "population"),
  abm_rules(made ~ made | fresh,
            style ~ case_when(!fresh ~ style, n_att > 0L ~ "resolution",
                              ever > 0L ~ "flight", TRUE ~ "oversight"),
            .scope = "population"),
  # a choice that has just been made solves everything still attached to it
  abm_tell(solved ~ TRUE, to = lapply(members, unlist),
           when = .group == "choices" & fresh),
  abm_rules(deficit ~ pmax(0, need * (base + n_att) - energy), .scope = "population")
)

result <- abm_run(m, go, ticks = 20, seed = 1)
```

**Result** (20 periods, 20 seeds. Counts out of 10 choices and 20 problems)

| access | load | decided | resolution | flight | oversight | problems solved |
|---|---|---|---|---|---|---|
| unsegmented | 0.5 | 10.0 | 1.0 | 0.0 | 9.0 | 20.0 |
| unsegmented | 1.1 | 9.0 | 0.0 | 9.0 | 0.0 | 0.0 |
| unsegmented | 2.2 | 6.0 | 0.0 | 6.0 | 0.0 | 0.0 |
| unsegmented | 3.3 | 4.0 | 0.0 | 4.0 | 0.0 | 0.0 |
| specialized | 0.5 | 8.6 | 1.9 | 5.8 | 0.9 | 8.0 |
| specialized | 1.1 | 7.1 | 0.1 | 6.7 | 0.3 | 0.1 |
| specialized | 2.2 | 5.0 | 0.0 | 4.8 | 0.2 | 0.0 |
| specialized | 3.3 | 1.1 | 0.0 | 0.9 | 0.1 | 0.0 |

Cohen, March and Olsen's claim is that decisions and problems are only loosely
coupled, and the table is that claim in one line: at a load of 1.1 the
organisation makes nine decisions out of ten and solves none of its twenty
problems. Every one of those nine is a *flight*, the problems that had been
attached to it moved somewhere else, and the choice was made once it was empty
enough to be cheap. Raising the load does not make the organisation deal with
its problems. It makes it decide less often, and still solve nothing.

The light-load row shows the other half of it. All ten choices are made and all
twenty problems are solved, but only *one* decision resolved anything: the
problems herd onto whichever choice is currently cheapest, so twenty of them
end up in the same can and are cleared at once, while the other nine choices
are made by oversight before any problem reaches them. "The garbage can" is
not a metaphor for disorder. It is the observation that a choice opportunity is
a container, and what ends up in it depends on what else is open at the time.

Segmenting access, so that a problem may only attach to a choice of its own
kind, spreads the problems out and puts some resolution back at light load, at
the cost of making fewer decisions overall.

*Forced `cost =` on `abm_match(pair = "nearest")`. The attachment rule is "go
to the accessible choice with the smallest energy deficit", and the old
`nearest` mode could only ask "which candidate is closest in these
coordinates". A deficit is a number the candidate carries, not a position, and
the access structure is a condition on the pair rather than on either agent.
`cost = if_else(open | kind == own_kind, deficit, NA_real_)` says both at once,
with `NA` meaning "not acceptable to me". The Open items entry asking for this
was written against Hotelling with prices. The garbage can is the model that
made it unavoidable, because there is no coordinate system in it at all.*

*Forced `abm_tell(to = <a set>)` and `.resolve = "collect"` together, and the
two turn out to be one idea. A choice does not know which problems are attached
to it, attachment is a column on the problem, pointing the other way, so the
problems tell it: `abm_tell(members ~ .id, to = .partner, .resolve = "collect")`
hands the choice the list of everyone who wrote to it. Later,
`abm_tell(solved ~ TRUE, to = lapply(members, unlist))` writes back to all of
them. One step gathers a set, the other addresses one. Before this round
`abm_tell()` could name exactly one recipient and could only combine several
senders into a number, which meant a many-to-one relation could be counted but
not held.*

*The parameterisation is ours, not Cohen, March and Olsen's. Their numeric
specification, the energy each choice requires in its own right, the exact
entry times, the three access and three decision structures, was not available
to us, so what is reproduced here is the mechanism and the qualitative
signature it produces, not the tables in the paper. The load parameter is the
ratio of total problem energy required to total decision-maker energy
available, which is their "net energy load". The rest is calibrated so that the
light-load and heavy-load regimes are both visible in twenty periods.*

---

**Reproduce:** [`m48_garbage_can.R`](scripts/m48_garbage_can.R)

← [47. Response thresholds and the division of labour](47-response-thresholds-and-division-of-labour.md) · [all models](README.md) · [49. The emergence of firms](49-emergence-of-firms.md) →
