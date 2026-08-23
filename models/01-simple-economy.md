# 1. Simple Economy (Wilensky & Rand, ch. 2)

**Concept**

- Setup: 500 agents, each with `money = 100`
- Go: everyone with money gives $1 to someone else
- Output: wealth distribution goes from a spike at 100 to an exponential tail

**NetLogo**

```netlogo
to go
  ask turtles [ if money > 0 [
    let recipient one-of other turtles
    set money money - 1
    ask recipient [ set money money + 1 ] ] ]
  tick
end
```

**Package**

```r
economy <- abm_setup(agents = abm_agents(n = 500, money = 100))

go <- abm_go(
  abm_match(pair = "random", role = list(giver = money > 0, receiver = TRUE)),
  abm_rules(money ~ if_else(.role == "giver", money - 1, money + 1))
)

result <- abm_run(economy, go, ticks = 1000, seed = 1)
```

*Introduced `role`. A transfer needs a direction, and the two conditions say which
agent in each pair can take which side; a pair where neither can give is dropped.*

---

[all models](README.md) · [2. El Farol, short form](02-el-farol-short.md) →
