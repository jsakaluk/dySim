# Expand pop_values_x Into Scenarios

Converts a tiered structure into a tibble where each row is one
scenario's population values. Supports: (1) fixed params supplied once
and recycled, (2) condition bundles for mathematically dependent params
(e.g. loadings + residuals), and (3) cross factors that vary
independently. When `conditions` or cross factors are named lists, names
become label columns for grouping.

## Usage

``` r
expand_pop_values_x(pop_values_x, labels = NULL)
```

## Arguments

- pop_values_x:

  Named list with three optional tiers:

  - **Fixed**: Elements that are a single value or vector (supply once,
    recycled into every scenario).

  - **Bundles** (`conditions`): A list of named lists; each sublist is a
    coherent condition (e.g. loadings + residuals that must stay
    paired). Use names for labels: `list(weak = ..., strong = ...)`.

  - **Cross factors**: Elements that are lists of scalars/vectors (e.g.
    `lv_cov = list(low = 0.3, med = 0.5, high = 0.8)`). Names become
    label columns.

- labels:

  Optional named list of label vectors for conditions and cross factors
  when they are unnamed (e.g.
  `list(conditions = c("weak","strong"), lv_cov = c("low","med","high"))`).
  Used as fallback when names are missing.

## Value

Tibble with `pop_values_x` list-column plus `condition_label`, scalar
columns for cross factor values, and `<factor>_label` columns when names
or labels are provided.

## See also

Other design:
[`create_design()`](https://jsakaluk.github.io/dySim/reference/create_design.md),
[`create_modelchoice_design()`](https://jsakaluk.github.io/dySim/reference/create_modelchoice_design.md)

## Examples

``` r
# Fixed + cross factor only (no bundles)
expand_pop_values_x(list(
  loadings_p1 = c(.75, .80, .55),
  loadings_p2 = c(.75, .80, .55),
  residuals_p1 = c(.44, .36, .70),
  residuals_p2 = c(.44, .36, .70),
  coresids = c(.1, .12, .08),
  lv_cov = list(0.3, 0.5, 0.8)
))
#> # A tibble: 3 × 2
#>   pop_values_x     lv_cov
#>   <list>            <dbl>
#> 1 <named list [6]>    0.3
#> 2 <named list [6]>    0.5
#> 3 <named list [6]>    0.8

# Fixed + bundles + cross factor (dependent measurement conditions)
expand_pop_values_x(list(
  intercepts = rep(0, 3),
  coresids = c(.1, .12, .08),
  conditions = list(
    list(loadings_p1 = c(.35,.4,.37), loadings_p2 = c(.35,.4,.37),
         residuals_p1 = c(.8775,.84,.8631), residuals_p2 = c(.8775,.84,.8631)),
    list(loadings_p1 = c(.7,.83,.87), loadings_p2 = c(.7,.83,.87),
         residuals_p1 = c(.49,.311,.243), residuals_p2 = c(.49,.311,.243))
  ),
  lv_cov = list(0.3, 0.5, 0.8)
))
#> # A tibble: 6 × 3
#>   pop_values_x     condition_label lv_cov
#>   <list>           <chr>            <dbl>
#> 1 <named list [7]> condition_1        0.3
#> 2 <named list [7]> condition_1        0.5
#> 3 <named list [7]> condition_1        0.8
#> 4 <named list [7]> condition_2        0.3
#> 5 <named list [7]> condition_2        0.5
#> 6 <named list [7]> condition_2        0.8

# Full bundle (conditions only; everything in each sublist)
expand_pop_values_x(list(
  conditions = list(
    list(loadings_p1 = c(.75,.8,.55), residuals_p1 = c(.44,.36,.7),
         coresids = c(.1,.12,.08), lv_cov = 0.3),
    list(loadings_p1 = c(.7,.83,.87), residuals_p1 = c(.49,.31,.24),
         coresids = c(.1,.12,.08), lv_cov = 0.5)
  )
))
#> # A tibble: 2 × 2
#>   pop_values_x     condition_label
#>   <list>           <chr>          
#> 1 <named list [4]> condition_1    
#> 2 <named list [4]> condition_2    

# Named conditions and cross factors yield label columns for grouping
expand_pop_values_x(list(
  conditions = list(weak = list(loadings_p1 = c(.35,.4), loadings_p2 = c(.35,.4)),
                   strong = list(loadings_p1 = c(.7,.8), loadings_p2 = c(.7,.8))),
  lv_cov = list(low = 0.3, med = 0.5, high = 0.8)
))
#> # A tibble: 6 × 4
#>   pop_values_x     condition_label lv_cov lv_cov_label
#>   <list>           <chr>            <dbl> <chr>       
#> 1 <named list [3]> weak               0.3 low         
#> 2 <named list [3]> weak               0.5 med         
#> 3 <named list [3]> weak               0.8 high        
#> 4 <named list [3]> strong             0.3 low         
#> 5 <named list [3]> strong             0.5 med         
#> 6 <named list [3]> strong             0.8 high        
```
