# Coding Style

## Overview
HAFS (Hurricane Analysis and Forecast System) is the UFS (Unified Forecast
System) application specialized in tropical cyclone research and forecasting.
The HAFS application development targets a common end-to-end HAFS
application/workflow to support both research and operational hurricane
applications. With that, the HAFS application/workflow needs to be compliant
with the operational standards (including [NCO Implementation
Standards](https://www.nco.ncep.noaa.gov/idsb/implementation_standards/) and
NCEP EE2 Guidance) for easy operational transition and implementation.
Meanwhile, we strive to design and implement the HAFS application workflow so
that it is easy to learn and use, promoting community modeling and development.

## General

* Trim all trailing whitespace from every line (here is an example `sed` command, 
  `sed -i -e 's/[ \t]*$//' filename`)
* No tab characters unless absolutely necessary
* Do not add binary files/executables, or NetCDF input files to the repository
* Avoid adding non-existing symbolic links to the repository
* Follow the coding style of the current file, as much as possible

## Python

* Python3.6 or newer
* Follow the coding style of the current file and/or files under the same directory

## Shell

* Using Bash is preferred
* Two space indentation, no tabs
* Put `; do` and `; then` on the same line as the `while`, `for` or `if`
* Use `$(command)` instead of backticks
* `[[ … ]]` is preferred over `[ … ]`, `test` and `/usr/bin/[`
* Use `(( … ))` or `$(( … ))` rather than `let` or `$[ … ]` or `expr`
* Follow the coding style of the current file and/or files under the same directory

## Fortran

* Use Fortran 95 standard or newer
* Two space indentation, no tabs
* Lines must be <= 120 characters long (including comments)
* Always specify `implicit none` (never use implicit variables)
* `COMMON` blocks should never be used
* Do not use `GOTO` statements
* Terminate `do` loops with `enddo`
* Terminate `if`, `then` statements with `endif`
* Avoid using Fortran reserved words as variables (e.g., `DATA`, `NAME`)
* Provide detailed descriptions of modules, interfaces, functions, and subroutines
* Define all function/subroutine arguments, and function results
* All OpenMP directives should specify default(none), and then explicitly list
  all shared and private variables, and all critical sections must have a unique name
* Follow the coding style of the current file and/or files under the same directory

