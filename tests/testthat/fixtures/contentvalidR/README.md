# contentvalidR handoff fixtures

Fifteen handoff objects produced by contentvalidR itself, for testing a reader
without contentvalidR installed. Each is genuine producer output from a release
tag, never edited afterwards. `MANIFEST.csv` records each file's tag, the
commit SHA it was built from, and its md5. The 0.6.0 and 0.7.0 files delivered
on 2026-09-23 and the 0.8.0 files delivered on 2026-09-26 are unchanged, byte
for byte.

| fit | 0.6.0 | 0.7.0 | 0.8.0 | 0.9.0 | what it exercises |
| --- | --- | --- | --- | --- | --- |
| `walkthrough-sort` | plain | `keying` + `response_min/max` set | as 0.7.0 | as 0.8.0 | the walkthrough item sort: 12 items, 10 carried, 2 held back; EF2 and TF2 reverse-worded (`keying = -1`), scale 1-5 |
| `expert-krippendorff` | yes | yes, keying/scale `NA` | as 0.7.0 | as 0.8.0 | a relevance panel with one `panel_statistics` row (Krippendorff's alpha with a bootstrap interval) |
| `delphi` | yes | yes, keying/scale `NA` | as 0.7.0 | as 0.8.0 | a three-round Delphi; `round` varies by item |
| `expert-nine` | — | yes | yes | as 0.8.0 | nine experts, one item per count relevant (9, 8, 7, 6); the one input whose decision depends on the producer |

**0.9.0 files are identical to their 0.8.0 counterparts** in every field
except `provenance$package_version` and `provenance$created`: the same schema
version, fields, columns, column classes, values, prose, and decisions. 0.9.0
changed what contentvalidR prints, not what it hands off.

## What a reader should find

- **0.6.0 files have no `note` column and no `keying`, `response_min`, or
  `response_max`.** A schema-version-1 reader must treat them as absent, not
  as an error.
- **0.7.0 and 0.8.0 files carry all four.** In `walkthrough-sort` keying and
  the scale are set; elsewhere they are `NA` throughout, meaning unknown.
- **Every 0.8.0 file is schema version 1 with the same fields and columns as
  0.7.0.** 0.8.0 changed no field, no column, and no type. What differs
  between a 0.7.0 file and its 0.8.0 counterpart is only what the frozen
  schema leaves free to change:
  - prose: the `rule` text (now counts and APA numbers, citing Lynn, 1986),
    one Delphi `note` (S5 now says "the share who kept their rating" where it
    said `prop_unchanged`), and `provenance$citation` (Lynn, 1986, added);
  - the value of `item_statistics$criterion` for an I-CVI, which is now
    Lynn's exact proportion for the panel size (.875 for eight experts, where
    0.7.0 stored .78 for every panel of six or more);
  - in `expert-nine` only, one item's decision (below);
  - `provenance$package_version` and `provenance$created`.
- **No decision changes in the three original fits.**

## The nine-expert case

| item | experts rating it relevant | I-CVI | 0.7.0 | 0.8.0 |
| --- | --- | --- | --- | --- |
| N9 | 9 of 9 | 1.00 | carried (Strong support) | carried (Strong support) |
| N8 | 8 of 9 | .89 | carried (Strong support) | carried (Strong support) |
| N7 | 7 of 9 | .78 | **held back (Review)** | **carried (Strong support)** |
| N6 | 6 of 9 | .67 | held back (Review) | held back (Review) |

Lynn's (1986) Table 2 requires 7 of 9 experts. Up to 0.7.0 the package
compared the I-CVI with a rounded .78, which 7/9 = .778 misses. 0.8.0 compares
counts, so N7 is carried. `items` is `N9 N8` from 0.7.0 and `N9 N8 N7` from
0.8.0. The same correction applies to 7/9 of any panel that is a multiple of
nine, such as 14 of 18.

A reader should not infer anything from the producer version beyond what the
object says: the decision is in `status`, `recommendation`, and `carried`,
and the rule that made it is in `rule`.

## The Delphi cases

| item | last round | weighted kappa | proportion unchanged | `note` (0.7.0 on) |
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
`provenance$created` (and so in md5). Rerunning it at v0.7.0 on 2026-09-26
reproduced the three delivered 0.7.0 files exactly apart from that field.
Because the script now includes `expert-nine`, a run at v0.6.0 would also
write an `expert-nine-v0.6.0` file; it is not part of this set.

Checked before handoff, in an R process where contentvalidR was not loaded:
every 0.8.0 file reads back as a `cv_handoff` at schema version 1 with the
producer version its filename says.
