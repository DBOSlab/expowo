# Fastly save CSV file within a current date folder

Uses `fwrite` to fastly write a CSV file within a current date subfolder
of a provided specific directory.

## Usage

``` r
saveCSV(df,
        dir = "results",
        filename = "output",
        verbose = TRUE,
        append = FALSE,
        save = TRUE,
        foldername = NULL)
```

## Arguments

- df:

  Any input list of same length vectors (e.g. `data.frame` and
  `data.table`).

- dir:

  Pathway to the computer's directory, where the file will be saved
  provided that the argument `save` is set up in `TRUE`. The default is
  to create a directory named **results** and the input `data.frame`
  will be saved within a subfolder named after the current date.

- filename:

  Name of the output file to be saved. The default is to create a file
  entitled **output**.

- verbose:

  Logical, if `FALSE`, a message showing the saving process will not be
  printed in the console in full.

- append:

  Logical, if `TRUE`, the file is opened in append mode and column names
  (header row) are not written.

- save:

  Logical, if `FALSE`, the input `data.frame` will be not be saved on
  disk.

- foldername:

  A parameter only for internal use with
  [expowo](https://dboslab.github.io/expowo/)'s main functions. You must
  keep this always as `NULL`.

## Author

Debora Zuanny & Domingos Cardoso

## Examples

``` r
if (FALSE) { # \dontrun{
library(expowo)

saveCSV(angioData,
        dir = "results_Angiosperms",
        filename = "Angiosperms_expowo",
        append = FALSE)
} # }
```
