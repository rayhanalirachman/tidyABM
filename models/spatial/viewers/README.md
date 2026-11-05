# The spatial models, played back

One HTML file per model, written by the model's script with
`abm_view_lattice()`. Each is self-contained — the script, the style and the
recorded frames are inline, nothing is fetched — so it opens straight from
disk: download it (GitHub shows the source, not the page) and open it in any
browser, on a laptop or a phone. It follows the system's light or dark theme
and has a switch of its own.

| viewer | what it plays | frames |
|---|---|---|
| [`01-life.html`](01-life.html) | a 60×60 random soup at density 0.30, every tick | 201 |
| [`03-fire.html`](03-fire.html) | a 60×60 forest at density 0.63 lit along the left edge, every other tick until the fire is out | ~100 |
| [`04-schelling.html`](04-schelling.html) | the 51×51 town at tolerance 0.30, every tick; hover for a cell's same-type share | 101 |
| [`05-wolf-sheep.html`](05-wolf-sheep.html) | the standard regime, grass shaded by regrowth with sheep and wolves on it, every other tick; hover for an animal's energy | 201 |
| [`06-ants.html`](06-ants.html) | the pheromone field with the nest and food marked and the ants coloured by whether they are carrying food, every other tick | 201 |
| [`08-sugarscape.html`](08-sugarscape.html) | the sugar on each cell with the agents on it, every fifth tick; hover for an agent's wealth, vision and metabolism | 101 |
| [`13-rebellion.html`](13-rebellion.html) | legitimacy 0.82, every tick: citizens quiet, active or jailed, and the cops | 201 |

**Controls.** Play / pause, step one tick either way, jump to either end, a
slider, and a speed from ¼× to 8×. The keyboard does the same: <kbd>space</kbd>
plays, <kbd>←</kbd> <kbd>→</kbd> step, <kbd>shift</kbd> + arrow steps ten,
<kbd>Home</kbd> / <kbd>End</kbd> jump. Hovering (or tapping) a cell writes it
out below the canvas: its coordinates, its values, and any agent standing on
it. The legend counts each level at the current tick.

**Size.** A frame's cells are quantised to at most 64 levels and run-length
encoded, and an agent costs a few bytes a tick, which is why these are a few
hundred kilobytes each rather than tens of megabytes. Thin a long run with
`record =` in `abm_run()` or `tick =` in `abm_view_lattice()`.
