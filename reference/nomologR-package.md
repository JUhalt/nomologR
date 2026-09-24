# nomologR: Guided scale development and construct validation

`nomologR` coordinates established psychometric and structural-equation
modeling tools while adding evidence-guided diagnostics, transparent
decision logging, and teaching-oriented interpretation.

## Details

The package follows a measurement-first philosophy: understand the item
set and measurement model before interpreting a theory-specified
structural network. Numerical reference values are prompts for
investigation rather than universal deletion or validity rules.

## Stability and deprecation

From version 0.3.0, the first CRAN release, the public interface changes
only after a deprecation period. Code written against one release keeps
working in the next.

**What is covered.** The interface is:

- the exported functions;

- their documented arguments and defaults;

- the documented fields of the objects they return;

- the `type` values
  [`nomo_table()`](https://juhalt.github.io/nomologR/reference/nomo_table.md)
  accepts;

- the columns of decision logs.

Functions reached only with `:::`, undocumented fields, the wording of
decision-log rows, and the layout of rendered reports are not covered.

**Deprecation before removal.** A breaking change is deprecated for at
least one minor release before it takes effect. Breaking changes include
removing or renaming any part of the interface, or changing a default in
a way that changes results. While deprecated, the function or argument
keeps working, warns once per session naming its replacement, and is
listed in NEWS.

**What is not breaking.** Any release may add new functions, new
arguments whose defaults leave results unchanged, new fields in returned
objects, or new decision-log rows. Address fields by name, not position.

**Experimental.** Two parts of the interface are experimental until
1.0.0 and may change without a deprecation period, with the change
described in NEWS:

- [`nomo_missing()`](https://juhalt.github.io/nomologR/reference/nomo_missing.md).
  Its rule for flagging a difference may be refined to separate sampling
  variability from bias.

- The layout of
  [`nomo_apa_table()`](https://juhalt.github.io/nomologR/reference/nomo_apa_table.md)
  tables, which may be adjusted as APA style is applied to more tables.

**The contentvalidR handoff.** The exchange object is versioned by its
producer. Within a schema version, fields are only added, and nomologR
ignores fields it does not know. Anything else is a new schema version,
agreed with `contentvalidR`. nomologR refuses a schema version it does
not read, naming both package versions.

## See also

Useful links:

- <https://github.com/JUhalt/nomologR>

- <https://juhalt.github.io/nomologR/>

- Report bugs at <https://github.com/JUhalt/nomologR/issues>

## Author

**Maintainer**: Joshua Uhalt <Josh.Uhalt@gmail.com>

Authors:

- Joshua Uhalt <Josh.Uhalt@gmail.com>
