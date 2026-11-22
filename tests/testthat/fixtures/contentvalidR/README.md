# contentvalidR handoff fixtures

Forty-two handoff objects produced by contentvalidR itself, for testing a
reader without contentvalidR installed. Each is genuine producer output, never
edited afterwards: from a release tag for 0.6.0 through 0.10.1, from the
release candidate's commit for 0.99.0, and from the 1.0.0 commit for 1.0.0.
`MANIFEST.csv` records each file's tag, the commit SHA it was built from, and
its md5. No delivery changed a file stored before it, byte for byte. The five
0.10.0 files come from the
`v0.10.0` tag (aff485a), and the five 0.10.1 files from the `v0.10.1` tag
(4c13be0), both generated on 2026-09-28 with the same script. The eight 0.99.0
files were generated from untagged commit 3766519 (DESCRIPTION 0.99.0),
contentvalidR's release candidate on its master, by contentvalidR on
2026-11-21 with this folder's script; nomologR generated them again from the
same commit and found the same objects (see "Regenerating"). That commit was
later tagged `v1.0.0-rc.1`, so the manifest gives that tag for the eight files.
The eight 1.0.0 files were made the same way on the same day, from untagged
commit f75d82f (DESCRIPTION 1.0.0), contentvalidR's 1.0.0 on its master, and
nomologR again found the same objects. That commit was later tagged `v1.0.0`,
so the manifest gives that tag for the eight files.

| fit | 0.6.0 | 0.7.0 | 0.8.0 | 0.9.0 | 0.10.0 | 0.10.1 | 0.99.0 | 1.0.0 | what it exercises |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| `walkthrough-sort` | plain | `keying` + `response_min/max` set | as 0.7.0 | as 0.8.0 | as 0.9.0 | as 0.10.0 | `n_constructs_given` added | as 0.99.0 | the walkthrough item sort: 12 items, 10 carried, 2 held back; EF2 and TF2 reverse-worded (`keying = -1`), scale 1-5 |
| `expert-krippendorff` | yes | yes, keying/scale `NA` | as 0.7.0 | as 0.8.0 | as 0.9.0 | as 0.10.0 | `rule` and `citation` reworded | as 0.99.0 | a relevance panel with one `panel_statistics` row (Krippendorff's alpha with a bootstrap interval) |
| `delphi` | yes | yes, keying/scale `NA` | as 0.7.0 | as 0.8.0 | as 0.9.0 | as 0.10.0 | `rule` and `citation` reworded, one `note` added | as 0.99.0 | a three-round Delphi; `round` varies by item |
| `expert-nine` | — | yes | yes | as 0.8.0 | as 0.9.0 | as 0.10.0 | `rule` and `citation` reworded | as 0.99.0 | nine experts, one item per count relevant (9, 8, 7, 6); the one input whose decision depends on the producer |
| `walkthrough-sort-none-reversed` | — | — | — | yes | as 0.9.0 | as 0.10.0 | `n_constructs_given` added | as 0.99.0 | the same sort with `reverse_keyed = character(0)` and `response_scale = c(1, 5)`: keying recorded as checked, no item reversed |
| `congruence-targeted` | — | — | — | — | — | — | yes | as 0.99.0 | item-objective congruence with a target mapping: five experts, items C1, C2 and C3 written for objectives A, B and C; C2 falls below the criterion and is held back (`items` is `C1 C3`) |
| `congruence-untargeted` | — | — | — | — | — | — | yes | as 0.99.0 | the same ratings without a target mapping, so every item is "Descriptive only", handed off with `keep = c("Supported", "Descriptive only")` (`items` is `C1 C2 C3`, no scales) |
| `congruence-untargeted-default` | — | — | — | — | — | — | yes | as 0.99.0 | the same fit with the default `keep = "Supported"`, which carries no item; a reader refuses it |

**0.9.0 files are identical to their 0.8.0 counterparts** in every field
except `provenance$package_version` and `provenance$created`: the same schema
version, fields, columns, column classes, values, prose, and decisions. 0.9.0
changed what contentvalidR prints, not what it hands off.

**0.10.0 files are identical to their 0.9.0 counterparts** in every field
except `provenance$package_version` and `provenance$created`, checked in an R
process without contentvalidR loaded. 0.10.0 added figures and documentation;
the one handoff change in it, the reworded `citation` for
`stability = "percent_change"`, reaches none of these fits.

**0.10.1 files are identical to their 0.10.0 counterparts** in the same
way: every field but `provenance$package_version` and `provenance$created`.
0.10.1 changed documentation only.

**0.99.0 files keep every decision of their 0.10.1 counterparts**: the same
schema version, items, scales, statuses, recommendations, carry decisions,
statistics, columns, and column classes. Besides `provenance$package_version`
and `provenance$created`, they differ only in prose and in one new design
field:

- `item_evidence$rule` in `expert-krippendorff`, `expert-nine`, and `delphi`.
  The expert rules now say that Lynn's count "also puts modified kappa above
  .74" and cite Polit et al. (2007); the Delphi rule says the threshold is "as
  supplied" and that an item was "last rated" in its round.
- `provenance$citation` in the same three fits, which now cites "Polit et al.
  (2007)" where it named all three authors.
- One `item_statistics$note` in `delphi`: S1's weighted kappa, whose bootstrap
  interval has no width, now says why, where it was empty.
- `provenance$design$n_constructs_given`, new in both sort fits and `FALSE`
  there: `sort_validity()` was not given `n_constructs`, the number of
  constructs judges could choose among, so it counted the constructs the data
  use.

The three congruence fits start at 0.99.0, the first producer that hands off
the index of item-objective congruence with one row per item.

**1.0.0 files are identical to their 0.99.0 counterparts** in every field
except `provenance$package_version` and `provenance$created`. Between its
release candidate and 1.0.0, contentvalidR changed nothing these eight fits
hand off.

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

## Checked, none reversed

`walkthrough-sort-none-reversed` records that someone checked the keying and
found no reverse-worded item: `keying` is `1` for all twelve items, and the
response scale is 1 to 5. That is a different statement from `NA`, which
means nobody said. Apart from `keying` (where `walkthrough-sort` has `-1` for
EF2 and TF2) and `provenance$created`, each version's file is identical to
that version's `walkthrough-sort` file: the same items, decisions, statistics,
and settings. That holds for 0.9.0, 0.10.0, 0.10.1, 0.99.0, and 1.0.0. The first was
generated from v0.9.0, then the latest released producer. A run at v0.7.0 or
v0.8.0 would differ from it the way those versions' `walkthrough-sort` files
differ from the 0.9.0 one.

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
| S1 | 3 | 1 | 1 | — up to 0.10.1; from 0.99.0, its bootstrap interval (1 to 1) has no width because every resample of the experts gave the same value |
| S2 | 3 | `NA` | 1 | kappa undefined: every rating in one category in both rounds |
| S3 | 2 | 0.385 | 0.75 | — (set aside after consensus in round 2) |
| S4 | 1 | `NA` | `NA` | rated in only one round, so no pair of rounds |
| S5 | 3 | 0 | 0.75 | a unanimous round, so kappa is 0 however many kept their rating |

S2 and S4 are the two different reasons a stability statistic is `NA`, and
`proportion unchanged` tells them apart: 1 for S2, `NA` for S4. S1's weighted
kappa is the only statistic in the file whose interval has no width.

## Regenerating

`make-handoff-fixtures.R` embeds every input, so it runs from anywhere with
`pkgload` and a contentvalidR source tree checked out at the tag. Its header
gives the steps. A regenerated file differs from these only in
`provenance$created` (and so in md5). Rerunning it at v0.7.0 on 2026-09-26
reproduced the three delivered 0.7.0 files exactly apart from that field.
Because the script now includes `expert-nine`, a run at v0.6.0 would also
write an `expert-nine-v0.6.0` file, and a run at v0.7.0 or v0.8.0 a
`walkthrough-sort-none-reversed` file; neither is part of this set. Rerunning
the edited script at v0.9.0 reproduced the four earlier 0.9.0 files exactly
apart from `provenance$created`.

On 2026-10-09 the script gained three item-objective congruence fits for the
1.0 release candidate: `congruence-targeted` (three items, each written for
one of three objectives; one falls below the congruence criterion and is held
back), `congruence-untargeted` (the same ratings without the target mapping,
handed off with `keep = c("Supported", "Descriptive only")`), and
`congruence-untargeted-default` (the same fit with the default `keep`, which
carries no item). They need a producer that hands off the index of
item-objective congruence with one row per item, so the script writes them
from 0.99.0 on and skips them for older producers. Rerunning the edited script
at v0.10.1 wrote the same five files as before, identical to the stored 0.10.1
files apart from `provenance$created`. The 0.99.0 files are the first stored
for these fits.

For the 0.99.0 files, nomologR exported contentvalidR's release-candidate
commit with `git archive`, ran the script on it, and compared each of its eight
objects with the delivered one after setting `provenance$created` to `NA` in
both. All eight were identical. Both runs were on 2026-11-21, so the files
were also the same byte for byte. The 1.0.0 files were checked the same way
from commit f75d82f, in a process of their own, with the same result: eight
identical objects, the same byte for byte.

The third congruence fit was first named `congruence-untargeted-default-keep`,
and contentvalidR delivered its 0.99.0 file under that name. The path
`nomologR/tests/testthat/fixtures/contentvalidR/handoff-congruence-untargeted-default-keep-v0.99.0.rds`
is 101 bytes. A tarball stores a path portably only up to 100 bytes, and R CMD
check noted it. On 2026-11-21 the fit was renamed `congruence-untargeted-default`
in the script, whose only other change is a comment saying why, and the script
was run again on the same export. It wrote all eight files byte for byte as delivered, the renamed one
included, so the delivered bytes are stored under the new name. The manifest
test now checks every stored path against that limit; the longest, a 1.0.0
file's, is 97 bytes.

Checked before handoff, in an R process where contentvalidR was not loaded:
every 0.8.0 file reads back as a `cv_handoff` at schema version 1 with the
producer version its filename says.
