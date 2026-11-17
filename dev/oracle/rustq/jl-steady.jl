# von Neumann tax-evasion model in Agents.jl 7.1. Same rules as ../vonneumann.R.
# Run: julia --project=. vonneumann_julia.jl   -> julia_results.csv (audit_rate, rep, tick, mgtr)
using Agents, Random, Statistics, DataFrames, CSV

const TAX, PEN = 0.30, 0.45

@agent struct Taxpayer(GridAgent{2})
    kind::Symbol
    income::Float64
    risk::Float64
    belief::Float64
    share::Float64
    declared::Float64
    owed::Float64
    audited::Bool
end

function declare!(a, model)
    w, k, q = a.income, a.risk, a.belief
    a.declared =
        a.owed > 0 ? a.owed :
        a.kind == :honest ? w :
        a.kind == :imitating ? mean(b.share for b in nearby_agents(a, model, 1)) * w :
        q > TAX / PEN ? w :
        q < TAX / (TAX + (PEN - TAX) * exp(k * PEN * w)) ? 0.0 :
        clamp(w - log((1 - q) * TAX / (q * (PEN - TAX))) / (k * PEN), 0, w)
end

function settle!(a, model)
    w, x, q, rate = a.income, a.declared, a.belief, model.rate
    a.audited = x < w - 1e-9 && rand(abmrng(model)) < rate
    a.owed = a.audited ? TAX * (w - x) * (1 + PEN * w) : 0.0
    a.audited ? (a.belief = 1.0) :
    q > rate ? (a.belief = q - 0.2 < 0 ? rate : q - 0.2) : nothing
    a.share = x / w
end

function year!(model)
    foreach(a -> declare!(a, model), allagents(model))   # everyone reads last year's shares, then writes
    foreach(a -> settle!(a, model), allagents(model))
    model.mgtr = TAX * sum(a.declared for a in allagents(model)) / sum(a.income for a in allagents(model))
end

function economy(rate, seed)
    rng = Xoshiro(seed)
    model = StandardABM(Taxpayer, GridSpace((21, 21); periodic = true, metric = :manhattan);
                        model_step! = year!, rng, properties = Dict(:rate => rate, :mgtr => 0.0))
    kinds = shuffle(rng, [fill(:honest, 50); fill(:dishonest, 50); fill(:imitating, 341)])
    for (pos, kind) in zip(Iterators.product(1:21, 1:21), kinds)
        add_agent!(pos, Taxpayer, model, kind, 100rand(rng), rand(rng), 0.02, kind == :honest, 0.0, 0.0, false)
    end
    model
end

function sweep()
rows = DataFrame(audit_rate = Float64[], rep = Int[], tick = Int[], mgtr = Float64[])
for rate in [0, .02, .04, .06, .08, .10], rep in 1:25
    m = economy(rate, 1000 * round(Int, 100rate) + rep)
    for tick in 1:40
        step!(m)
        push!(rows, (rate, rep, tick, m.mgtr))
    end
end
rows
end
@time sweep()
@time sweep()
@time sweep()
