# nomologR Vision Charter

## Why this package exists

Scale development is often taught as a sequence of disconnected statistical procedures:
inspect alpha, run an EFA, delete weak items, run a CFA, report fit, and declare the measure valid.

That workflow is easy to execute badly because the difficult part is not running the analyses.
The difficult part is understanding why an analysis is appropriate, what evidence it contributes,
how one decision changes the next, and when a result should alter the theory rather than merely the model.

`nomologR` exists to make that reasoning visible.

## What nomologR is

A guided layer around established psychometric and SEM tools that:

- chooses or recommends methods appropriate to the data,
- explains the methodological rationale,
- produces transparent diagnostics,
- records researcher decisions,
- connects measurement evidence to theory,
- and teaches while it analyzes.

## What nomologR is not

- not a replacement for `psych`
- not a replacement for `lavaan`
- not a replacement for `semTools`
- not an automatic item-deletion algorithm
- not a score that declares a scale "valid"
- not a black-box SEM generator
- not a content-validity package
- not a Solomon-design package

## The defining question

At every stage, the package should help answer:

> **What does this result tell me about the claim that these observations measure the construct I say they measure, and what should I investigate next?**

## The core intellectual path

Concept → items → content evidence → dimensionality → measurement model → reliability →
construct-validity evidence → invariance/generalizability → theoretically predicted network →
continued accumulation of evidence.

`contentvalidR` primarily supports the front of this path.
`nomologR` primarily supports the empirical measurement and network portion.
`solomonR` addresses a separate experimental-design problem.

## Standard for recommendations

A recommendation in `nomologR` should have four parts:

1. **Observation** — what was found.
2. **Reason** — why it might matter.
3. **Options** — defensible choices available to the researcher.
4. **Consequence** — what each choice means for subsequent analysis.

Example:

> Item S4 has a primary loading of .37, below the teaching reference of .40.
> Its communality is .51 and it has no material cross-loading. This is weak evidence
> against the item, not an automatic deletion criterion. If the item represents unique
> theoretical content, retaining it may preserve construct breadth. If similar content is
> already represented by stronger items, removal may improve measurement precision.
> Compare both models and record the rationale for the decision.

That is the voice of `nomologR`.
