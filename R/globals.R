# Package-level tidy-evaluation bindings ---------------------------------------
#
# ggplot2 evaluates aesthetic names inside a data mask. Registering the names
# used by nomologR plotting methods prevents R CMD check from treating those
# data-column references as unresolved global variables.
#
# This list is intentionally presentation-only. It does not create package
# objects or alter the values stored in result objects.

utils::globalVariables(c(
  "level",
  "score_x2",
  "constraint",
  "relation",
  "concordance",
  "n",
  "primary_estimate",
  "validation_estimate",
  "replication_status",
  "constraint_display",
  "theory_lower_plot",
  "theory_upper_plot",
  "concordance_display",
  "panel_x",
  "replication_display"
))
