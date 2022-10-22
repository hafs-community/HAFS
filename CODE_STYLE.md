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

* Trim all trailing whitespace from every line (here is an example sed command, 
  `sed -i -e 's/[ \t]*$//' filename`)
* No tab characters unless absolutely necessary
* Follow the coding style of the current file, as much as possible

## Python

* Python3.6 or newer
* Follow the coding style of the current file and/or files under the same directory

## Shell

* Follow the coding style of the current file and/or files under the same directory

## Fortran

* Use Fortran 95 standard or newer
* Two space indentation, or follow the existing indentation in the source code
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

