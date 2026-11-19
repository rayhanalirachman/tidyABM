//! The column store (migration phase 3): a run's agent groups as typed columns in
//! R's own bit formats, the snapshots `record` keeps, and the stacking of those
//! snapshots into the long table `abm_run()` returns.
//!
//! This crate never holds an R object. Everything R-specific is the bridge's
//! handle type `H` (a preserved R object there, anything `Clone` in the tests):
//! a column R types do not map onto (a list column, a complex vector) is
//! `ColData::Opaque(H)`, a typed column's attributes (factor levels, a Date class,
//! names) travel as `attrs: Some(H)`, and `mirror` is the R vector the column was
//! last loaded from or exported as, which the bridge hands back unchanged while
//! the column is unchanged.
//!
//! Strings and names are codes into the bridge's string pool (one code per R
//! `CHARSXP`, so equal codes are R-identical strings); `NA_CODE` is `NA_character_`.
//! Doubles keep their bits (R's `NA_real_` payload included), integers and logicals
//! are `i32` with `NA_INT` for `NA`.

use std::sync::Arc;

/// R's `NA_integer_` and `NA` (logical): the smallest `i32`.
pub const NA_INT: i32 = i32::MIN;
/// The pool code of `NA_character_`.
pub const NA_CODE: u32 = u32::MAX;

/// The values of a typed column.
#[derive(Clone, Debug, PartialEq)]
pub enum Values {
    Real(Vec<f64>),
    Int(Vec<i32>),
    Lgl(Vec<i32>),
    Str(Vec<u32>),
}

impl Values {
    pub fn len(&self) -> usize {
        match self {
            Values::Real(v) => v.len(),
            Values::Int(v) | Values::Lgl(v) => v.len(),
            Values::Str(v) => v.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// An empty vector of the same type, with room for `cap` values.
    fn empty_like(&self, cap: usize) -> Values {
        match self {
            Values::Real(_) => Values::Real(Vec::with_capacity(cap)),
            Values::Int(_) => Values::Int(Vec::with_capacity(cap)),
            Values::Lgl(_) => Values::Lgl(Vec::with_capacity(cap)),
            Values::Str(_) => Values::Str(Vec::with_capacity(cap)),
        }
    }

    fn same_type(&self, other: &Values) -> bool {
        std::mem::discriminant(self) == std::mem::discriminant(other)
    }

    /// Append `other`, which has the same type.
    fn extend(&mut self, other: &Values) {
        match (self, other) {
            (Values::Real(a), Values::Real(b)) => a.extend_from_slice(b),
            (Values::Int(a), Values::Int(b)) | (Values::Lgl(a), Values::Lgl(b)) => {
                a.extend_from_slice(b)
            }
            (Values::Str(a), Values::Str(b)) => a.extend_from_slice(b),
            _ => unreachable!("extend: types checked by the caller"),
        }
    }

    /// Append `n` NAs of this type (`na_real` is R's `NA_real_`, passed in from R).
    fn push_na(&mut self, n: usize, na_real: f64) {
        match self {
            Values::Real(a) => a.resize(a.len() + n, na_real),
            Values::Int(a) | Values::Lgl(a) => a.resize(a.len() + n, NA_INT),
            Values::Str(a) => a.resize(a.len() + n, NA_CODE),
        }
    }
}

/// A column's data: typed values shared between the current state and the
/// snapshots (copy on write), or an R object the store only carries.
#[derive(Clone, Debug)]
pub enum ColData<H> {
    Typed(Arc<Values>),
    Opaque(H),
}

#[derive(Clone, Debug)]
pub struct Column<H> {
    /// Pool code of the column name.
    pub name: u32,
    pub data: ColData<H>,
    /// The column's R attributes, when it has any.
    pub attrs: Option<H>,
    /// The R vector this column was loaded from or last exported as.
    pub mirror: Option<H>,
}

impl<H> Column<H> {
    /// Typed, without attributes: what the stacking in [`assemble`] handles itself.
    fn bare_values(&self) -> Option<&Values> {
        match (&self.data, &self.attrs) {
            (ColData::Typed(v), None) => Some(v),
            _ => None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Group<H> {
    /// Pool code of the group's name in the run's list of groups.
    pub name: u32,
    pub nrow: usize,
    pub cols: Vec<Column<H>>,
    /// The group tibble's own attributes (names, row names, class).
    pub frame: Option<H>,
    /// The R tibble this group was loaded from or last exported as.
    pub mirror: Option<H>,
}

impl<H> Default for Group<H> {
    /// An empty placeholder (for taking a group out of the store while it is edited).
    fn default() -> Self {
        Group {
            name: 0,
            nrow: 0,
            cols: Vec::new(),
            frame: None,
            mirror: None,
        }
    }
}

impl<H: Clone> Group<H> {
    pub fn col(&self, name: u32) -> Option<&Column<H>> {
        self.cols.iter().find(|c| c.name == name)
    }

    /// A copy for a snapshot: the values are shared, the mirrors are not kept (a
    /// snapshot never hands its R vectors back, and keeping them would keep every
    /// recorded tick's R copy alive as well as the store's).
    fn recorded(&self) -> Group<H> {
        Group {
            name: self.name,
            nrow: self.nrow,
            cols: self
                .cols
                .iter()
                .map(|c| Column {
                    name: c.name,
                    data: c.data.clone(),
                    attrs: c.attrs.clone(),
                    mirror: None,
                })
                .collect(),
            frame: self.frame.clone(),
            mirror: None,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Snapshot<H> {
    pub tick: i32,
    pub groups: Vec<Group<H>>,
}

/// One run's state and record.
#[derive(Debug)]
pub struct Store<H> {
    pub groups: Vec<Group<H>>,
    /// Tick 0, kept whatever `record` says: an empty result takes its columns.
    pub first: Option<Snapshot<H>>,
    /// The recorded ticks, in order.
    pub snaps: Vec<Snapshot<H>>,
}

impl<H: Clone> Default for Store<H> {
    fn default() -> Self {
        Store {
            groups: Vec::new(),
            first: None,
            snaps: Vec::new(),
        }
    }
}

impl<H: Clone> Store<H> {
    /// Record the current groups as tick `tick` (`keep`: in the result; tick 0 is
    /// also kept as the template of an empty result).
    pub fn snapshot(&mut self, tick: i32, keep: bool) {
        let snap = || Snapshot {
            tick,
            groups: self.groups.iter().map(Group::recorded).collect(),
        };
        if tick == 0 && self.first.is_none() {
            self.first = Some(snap());
        }
        if keep {
            let s = snap();
            self.snaps.push(s);
        }
    }
}

/// The pool codes of the names [`assemble`] treats specially.
#[derive(Clone, Copy, Debug)]
pub struct Names {
    pub id: u32,
    pub group: u32,
    pub tick: u32,
    /// `""`
    pub blank: u32,
}

/// The recorded snapshots stacked into the columns of the run's long table, in the
/// order `abm_run()` returns them: `tick`, `.id`, `.group`, then every other
/// column in order of first appearance (tick by tick, group by group).
///
/// This is what `dplyr::bind_rows()` of the per-tick `vctrs::vec_rbind()` of the
/// groups gives when every column is a typed vector without attributes and each
/// name has one type wherever it appears: a column a group lacks is `NA` of that
/// type on its rows. Anything else (an opaque or classed column, a name with two
/// types, a column named `tick`, a duplicated, empty or `NA` name, no `.id` or
/// `.group`) returns `None`, and the bridge's caller stacks the snapshots with R's
/// own code. With no recorded tick the result is tick 0's columns with no rows.
pub fn assemble<H: Clone>(
    store: &Store<H>,
    names: Names,
    na_real: f64,
) -> Option<Vec<(u32, Values)>> {
    let (snaps, zero_rows): (&[Snapshot<H>], bool) = if store.snaps.is_empty() {
        (std::slice::from_ref(store.first.as_ref()?), true)
    } else {
        (&store.snaps, false)
    };

    // the union of the columns, and each name's prototype (its type)
    let mut order: Vec<u32> = Vec::new();
    let mut proto: Vec<&Values> = Vec::new();
    for s in snaps {
        for g in &s.groups {
            for (j, c) in g.cols.iter().enumerate() {
                let v = c.bare_values()?;
                if v.len() != g.nrow
                    || c.name == names.tick
                    || c.name == names.blank
                    || c.name == NA_CODE
                    || g.cols[..j].iter().any(|d| d.name == c.name)
                {
                    return None;
                }
                match order.iter().position(|&n| n == c.name) {
                    Some(k) if !proto[k].same_type(v) => return None,
                    Some(_) => {}
                    None => {
                        order.push(c.name);
                        proto.push(v);
                    }
                }
            }
        }
    }
    let at = |n: u32| order.iter().position(|&m| m == n);
    let (id, group) = (at(names.id)?, at(names.group)?);

    let rows_of = |s: &Snapshot<H>| -> usize {
        if zero_rows {
            0
        } else {
            s.groups.iter().map(|g| g.nrow).sum()
        }
    };
    let total: usize = snaps.iter().map(rows_of).sum();

    let mut tick = Vec::with_capacity(total);
    for s in snaps {
        tick.resize(tick.len() + rows_of(s), s.tick);
    }
    let mut out = vec![(names.tick, Values::Int(tick))];
    let mut idx = vec![id, group];
    idx.extend((0..order.len()).filter(|&k| k != id && k != group));
    for k in idx {
        let name = order[k];
        let mut v = proto[k].empty_like(total);
        if !zero_rows {
            for s in snaps {
                for g in &s.groups {
                    match g.col(name).and_then(Column::bare_values) {
                        Some(x) => v.extend(x),
                        None => v.push_na(g.nrow, na_real),
                    }
                }
            }
        }
        out.push((name, v));
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    const NAMES: Names = Names {
        id: 0,
        group: 1,
        tick: 2,
        blank: 3,
    };
    // R's NA_real_ bits, as R hands them over
    const NA_REAL: u64 = 0x7FF0_0000_0000_07A2;

    fn col(name: u32, v: Values) -> Column<u8> {
        Column {
            name,
            data: ColData::Typed(Arc::new(v)),
            attrs: None,
            mirror: Some(9),
        }
    }

    fn group(name: u32, cols: Vec<Column<u8>>) -> Group<u8> {
        let nrow = cols.first().map_or(0, |c| match &c.data {
            ColData::Typed(v) => v.len(),
            ColData::Opaque(_) => 0,
        });
        Group {
            name,
            nrow,
            cols,
            frame: None,
            mirror: Some(9),
        }
    }

    fn with_groups(groups: Vec<Group<u8>>) -> Store<u8> {
        Store {
            groups,
            ..Default::default()
        }
    }

    fn ids(v: &[i32]) -> Column<u8> {
        col(0, Values::Int(v.to_vec()))
    }

    fn grp(code: u32, n: usize) -> Column<u8> {
        col(1, Values::Str(vec![code; n]))
    }

    fn na() -> f64 {
        f64::from_bits(NA_REAL)
    }

    #[test]
    fn snapshots_share_values_and_drop_mirrors() {
        let mut s = with_groups(vec![group(10, vec![ids(&[1, 2]), grp(10, 2)])]);
        s.snapshot(0, false);
        s.snapshot(1, true);
        assert!(s.first.is_some());
        assert_eq!(s.snaps.len(), 1);
        let g = &s.snaps[0].groups[0];
        assert!(g.mirror.is_none() && g.cols.iter().all(|c| c.mirror.is_none()));
        match (&g.cols[0].data, &s.groups[0].cols[0].data) {
            (ColData::Typed(a), ColData::Typed(b)) => assert!(Arc::ptr_eq(a, b)),
            _ => panic!("typed expected"),
        }
    }

    #[test]
    fn union_order_and_typed_na_fill() {
        // group A: .id, .group, x (double); group B: .id, .group, y (logical), x
        let a = group(
            10,
            vec![
                ids(&[1, 2]),
                grp(10, 2),
                col(5, Values::Real(vec![0.5, -0.0])),
            ],
        );
        let b = group(
            11,
            vec![
                ids(&[3]),
                grp(11, 1),
                col(6, Values::Lgl(vec![1])),
                col(5, Values::Real(vec![2.0])),
            ],
        );
        let mut s = with_groups(vec![a, b]);
        s.snapshot(0, true);
        s.snapshot(1, true);
        let out = assemble(&s, NAMES, na()).unwrap();
        let names: Vec<u32> = out.iter().map(|c| c.0).collect();
        assert_eq!(names, vec![2, 0, 1, 5, 6]);
        assert_eq!(out[0].1, Values::Int(vec![0, 0, 0, 1, 1, 1]));
        assert_eq!(out[1].1, Values::Int(vec![1, 2, 3, 1, 2, 3]));
        assert_eq!(out[2].1, Values::Str(vec![10, 10, 11, 10, 10, 11]));
        match &out[3].1 {
            Values::Real(x) => {
                assert_eq!(x[1].to_bits(), (-0.0f64).to_bits(), "bits kept");
                assert_eq!(x.len(), 6);
            }
            _ => panic!(),
        }
        assert_eq!(
            out[4].1,
            Values::Lgl(vec![NA_INT, NA_INT, 1, NA_INT, NA_INT, 1])
        );
    }

    #[test]
    fn na_real_fill_uses_the_given_bits() {
        let a = group(10, vec![ids(&[1]), grp(10, 1)]);
        let b = group(
            11,
            vec![ids(&[2]), grp(11, 1), col(7, Values::Real(vec![1.0]))],
        );
        let mut s = with_groups(vec![a, b]);
        s.snapshot(0, true);
        let out = assemble(&s, NAMES, na()).unwrap();
        match &out[3].1 {
            Values::Real(x) => assert_eq!(x[0].to_bits(), NA_REAL),
            _ => panic!(),
        }
    }

    #[test]
    fn an_empty_record_is_tick_zero_without_rows() {
        let mut s = with_groups(vec![group(
            10,
            vec![
                ids(&[1, 2]),
                grp(10, 2),
                col(5, Values::Str(vec![NA_CODE, 4])),
            ],
        )]);
        s.snapshot(0, false);
        s.snapshot(1, false);
        let out = assemble(&s, NAMES, na()).unwrap();
        assert_eq!(out.len(), 4);
        assert!(out.iter().all(|c| c.1.is_empty()));
        assert_eq!(out[3].1, Values::Str(vec![]));
    }

    #[test]
    fn declines_what_r_must_stack() {
        let base = || vec![ids(&[1]), grp(10, 1)];
        let run = |groups: Vec<Group<u8>>| {
            let mut s = with_groups(groups);
            s.snapshot(0, true);
            assemble(&s, NAMES, na())
        };
        assert!(run(vec![group(10, base())]).is_some());
        // two types under one name
        let mut a = base();
        a.push(col(5, Values::Int(vec![1])));
        let mut b = base();
        b.push(col(5, Values::Real(vec![1.0])));
        assert!(run(vec![group(10, a), group(11, b)]).is_none());
        // a classed column
        let mut c = base();
        let mut f = col(5, Values::Int(vec![1]));
        f.attrs = Some(1);
        c.push(f);
        assert!(run(vec![group(10, c)]).is_none());
        // an opaque column
        let mut d = base();
        d.push(Column {
            name: 5,
            data: ColData::Opaque(1),
            attrs: None,
            mirror: None,
        });
        assert!(run(vec![group(10, d)]).is_none());
        // a column called tick, a blank name, a duplicated name, no .group
        for bad in [2u32, 3, NA_CODE, 0] {
            let mut e = base();
            e.push(col(bad, Values::Int(vec![1])));
            assert!(run(vec![group(10, e)]).is_none(), "name {bad}");
        }
        assert!(run(vec![group(10, vec![ids(&[1])])]).is_none());
        // a typed column of the wrong length (a matrix column has attributes, so
        // this cannot come from R, but the stacking must not trust it)
        let mut g = base();
        g.push(col(5, Values::Int(vec![1, 2])));
        assert!(run(vec![group(10, g)]).is_none());
    }
}
