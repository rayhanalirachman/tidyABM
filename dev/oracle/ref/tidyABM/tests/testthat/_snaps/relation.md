# abm_relation() keeps its value columns and fills declared defaults

    Code
      print(r)
    Message
      <abm_relation> 2 pairs
      * unmet: <numeric>, default 0
      * seen: <logical>, default FALSE

# abm_setup() validates relations against the population

    Code
      print(m)
    Message
      <abm_model> 7 agents in 2 groups
      * hh: 3 agents ["pick", "pi", "rat", and "got"]
      * f: 4 agents ["price", "n_buy", and "inv"]
      * relation sellers: 6 pairs ["unmet"]

# abm_pairs() updates every pair, sees both endpoints, and honours .when

    Code
      print(abm_pairs(via = "sellers", unmet ~ 0, .when = to_price > 1))
    Message
      <abm_pairs> via sellers
      * unmet ~ `0`
      * .when = `to_price > 1`

