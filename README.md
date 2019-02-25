## labellr

[![Travis-CI Build Status](https://travis-ci.org/oizin/labellr.svg?branch=master)](https://travis-ci.org/oizin/labellr)

Deterministic labelling of registry data

## Overview


## Installation

```r
# install from the Github repo
# install.packages("devtools")
devtools::install_github("oizin/labellr")
```

## Usage

The core function in `labellr` is `classify`.

```r
# read in a dataset
> clinic_data <- read.csv(file.path(ext_path, "clinic_data.csv"), stringsAsFactors = FALSE,na.strings = "")
> clinic_data
  clinic_date xray fmri surgery_date         complications
1   8/05/2014    y <NA>         <NA>                  <NA>
2   9/05/2014 <NA>    y         <NA>                  <NA>
3  10/05/2014    y <NA>         <NA>                  <NA>
4  11/05/2014    y <NA>         <NA>                  <NA>
5        <NA> <NA> <NA>   10/05/2014 soreness around wound
6        <NA> <NA> <NA>   11/05/2014          none of note
7        <NA> <NA> <NA>   12/05/2014                  <NA>

# read in definitions table
> tmt_def <- read.csv(file.path(ext_path, "treatment_definitions.csv"), stringsAsFactors = FALSE)
> print(tmt_def,quote=TRUE)
   treatment clinic_date   xray   fmri surgery_date complications
1 "datatype"      "date" "char" "char"       "date"        "char"
2     "xray"         "1"    "y"   "!y"          "0"            ""
3     "fmri"         "1"   "!y"    "y"          "0"            ""
4  "surgery"         "0"   "!y"   "!y"          "1"            ""

# classify data
> library(labellr)
> classify(clinic_data,tmt_def)
  treatment clinic_date xray fmri surgery_date         complications
1      xray   8/05/2014    y <NA>         <NA>                  <NA>
2      fmri   9/05/2014 <NA>    y         <NA>                  <NA>
3      xray  10/05/2014    y <NA>         <NA>                  <NA>
4      xray  11/05/2014    y <NA>         <NA>                  <NA>
5   surgery        <NA> <NA> <NA>   10/05/2014 soreness around wound
6   surgery        <NA> <NA> <NA>   11/05/2014          none of note
7   surgery        <NA> <NA> <NA>   12/05/2014                  <NA>
```

## Getting help

If you encounter a bug please open an issue on [Github](https://github.com/oizin/labellr/issues).