# Walkthrough data from contentvalidR

These three files are copied, unchanged, from
[contentvalidR](https://github.com/JUhalt/contentvalidR) at tag `v0.9.0`
(commit `c0d0696b4b40f3852fa3784beb864902368d8f2b`). They are the shared
teaching data of the joint walkthrough in #53: contentvalidR's
`vignette("one-item-set-both-stages")` takes the item set through content review,
and nomologR's article "From content review to empirical screening" takes the
same responses through the empirical screen.

| file | git blob hash at contentvalidR v0.9.0 |
| --- | --- |
| `walkthrough_responses.csv` | `c61b39ab6219f88e88517de11a9d4efc7600f3ab` |
| `walkthrough_items.csv` | `0dcd2572c74429c86d21735321d0887ee173f2f7` |
| `build-walkthrough-data.R` | `fedbf730ec7595b972e430f6d36f2a46e08b38a8` |

`git hash-object <file>` on each file reproduces its hash. The files are marked
`-text` in `.gitattributes`, so git never changes their line endings.

`build-walkthrough-data.R` is contentvalidR's generator. It is base R, seeded,
and documents the population model. Run from a contentvalidR source tree, it
writes the two CSVs. Checked on 2026-09-27 with R 4.6.1: the regenerated files
read back identical to these, differing only in line endings.

`../nomo_demo_walkthrough.R` converts the CSVs into the package datasets
`nomo_demo_walkthrough` and `nomo_demo_walkthrough_items`. If contentvalidR
changes the files, which it will announce in its NEWS and its release-candidate
fixture delivery, copy the new versions here, update the hashes, and rerun that
script.
