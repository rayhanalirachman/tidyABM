# 21. Genetic Drift / Wright–Fisher (NetLogo GenDrift P Global)

**Concept**

- Setup: 200 agents, each with one of five alleles
- Go: the whole population is redrawn from itself, uniformly with replacement
- Output: fixation is certain, the winner is random, and each allele wins with
  exactly its starting frequency

**NetLogo**

```netlogo
to go
  ask patches [ set pcolor [pcolor] of one-of patches ]
end
```

**Package**

```r
drift <- abm_setup(agents = abm_agents(n = 200, allele = ~sample(1:5, n, replace = TRUE)))

go <- abm_go(abm_rules(allele ~ sample(allele, n(), replace = TRUE)))

result <- abm_run(drift, go, ticks = 800, seed = 3)
```

**Result.** Five alleles → two by tick 200 → one by tick 800. Over 120 replicates
starting at 30/70, the minority allele fixed in 33% of runs. Theory says 30%.

*One line, because a rule with no match standing sees the whole population, so
`sample(allele, n(), replace = TRUE)` is literally the Wright–Fisher operator. The
model that most nearly writes itself.*

---

← [20. SIR on a network with recovery timers](20-sir-on-a-network-with-recovery-timers.md) · [all models](README.md) · [22. Hawks and Doves](22-hawks-and-doves.md) →
