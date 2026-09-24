# contentvalidR handoff fixtures

Six handoff objects produced by contentvalidR itself, for testing a reader
without contentvalidR installed. Each is genuine producer output from a release
tag, never edited afterwards; `MANIFEST.csv` records the tag, the commit SHA it
was built from, and each file's md5.

| fit | 0.6.0 | 0.7.0 | what it exercises |
| --- | --- | --- | --- |
| `walkthrough-sort` | plain | `keying` + `response_min/max` set | the walkthrough item sort: 12 items, 10 carried, 2 held back; EF2 and TF2 reverse-worded (`keying = -1`), scale 1-5 |
| `expert-krippendorff` | yes | yes, keying/scale `NA` | a relevance panel with one `panel_statistics` row (Krippendorff's alpha with a bootstrap interval) |
| `delphi` | yes | yes, keying/scale `NA` | a three-round Delphi; `round` varies by item |

## What a reader should find

- **0.6.0 files have no `note` column and no `keying`, `response_min`, or
  `response_max`.** A schema-version-1 reader must treat them as absent, not
  as an error. This is the only way to test that path, since 0.7.0 always
  emits them.
- **0.7.0 files carry all four.** In `walkthrough-sort` keying and the scale
  are set; in the other two they are `NA` throughout, meaning unknown. So both
  of the agreed invariants can be exercised: each column is `NA` for every item
  or none, and `response_min`/`response_max` are `NA` together.
- **Every value the two versions share is identical** within each pair. The
  only differences are the added columns and `provenance$package_version` and
  `provenance$created`.

## The Delphi cases

| item | last round | weighted kappa | proportion unchanged | 0.7.0 `note` |
| --- | --- | --- | --- | --- |
| S1 | 3 | 1 | 1 | — |
| S2 | 3 | `NA` | 1 | kappa undefined: every rating in one category in both rounds |
| S3 | 2 | 0.385 | 0.75 | — (set aside after consensus in round 2) |
| S4 | 1 | `NA` | `NA` | rated in only one round, so no pair of rounds |
| S5 | 3 | 0 | 0.75 | a unanimous round, so kappa is 0 however many kept their rating |

S2 and S4 are the two different reasons a stability statistic is `NA`, and
`proportion unchanged` tells them apart: 1 for S2, `NA` for S4.

## Regenerating

`make-handoff-fixtures.R` embeds every input, so it runs from anywhere with
`pkgload` and a contentvalidR source tree checked out at the tag. Its header
gives the steps. A regenerated file differs from these only in
`provenance$created` (and so in md5).

Checked before handoff, in an R process where contentvalidR could not be
found: every file reads back as a `cv_handoff` at schema version 1 with the
producer version its filename says, and holds no environments, functions, or
pointers. The embedded walkthrough input equals the CSV that 0.7.0 ships.
