# Truncation-Induced Depenency in Summary Statistics (TIDES)

## A method to check for inconsistencies in reported summary statistics for truncated data



## Cite as

Hussey, I., Norwood, S. F., Cummins, J., Arslan, R. A., & Elson, M. (2024). Truncation-Induced Dependency in Summary Statistics (TIDES): A method to check for inconsistencies in reported summary statistics for truncated data. https://github.com/ianhussey/TIDES



## TODO

- consider use of round() in sd_bounds() and tides().
- Wallrich suggests that sd_bounds()'s `max_beta  <- min(max(max, min + 1, max_alpha + 1), max)` could be simplified to `max_beta  <- max`. Seems to work, but needs through testing before adopting the change.
- n_items doesn't seem to work appropriately above 1.
- how to get vignette to appear in documentation?
- improve vignettes tables, either select columns or change output rendering format
- should tides function be renamed so package and function aren't identical
- Differentiate mean based range violations from SD based violations in the output 
