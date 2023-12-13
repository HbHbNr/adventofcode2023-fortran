[![Fortran 2018](https://hbhbnr.github.io/badges/Fortran-2018-blue-fortran-white.svg)](https://fortran-lang.org/)
[![GitHub Workflow Status](https://github.com/HbHbNr/adventofcode2023-fortran/actions/workflows/codequality.yml/badge.svg)](https://github.com/HbHbNr/adventofcode2023-fortran/actions/workflows/codequality.yml)

# adventofcode2023-fortran
Solutions for https://adventofcode.com/2023/ in Fortran with unit testing.

# Solution status
| **Day**      | **1** | **2** | **3** | **4** | **5** | **6** | **7** | **8** | **9** | **10** | **11** | **12** | **13** |
|-------------:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:-----:|:------:|:------:|:------:|:------:|
| **Part One** | ✓     | ✓     | ✓       | ✓     | ✓     | ✓     | ✓     | ✓     | ✓     | ✓      | ✓      |        | ✓      |
| **Part Two** | ✓     | ✓     | ✓       | ✓     | ✓     | ✓     | ✓     | ✓     | ✓     | ✓      | ✓      |        | ✓      |

| **Day**      | **14** | **15** | **16** | **17** | **18** | **19** | **20** | **21** | **22** | **23** | **24** | **25** |
|-------------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|:------:|
| **Part One** |        |        |        |        |        |        |         |        |        |        |        |        |
| **Part Two** |        |        |        |        |        |        |         |        |        |        |        |        |

# Requirements
* [GFortran](https://gcc.gnu.org/wiki/GFortran)
* [fortran-regex](https://github.com/perazz/fortran-regex), downloaded by Makefile

Optional for unit testing with the FORTRAN Unit Test Framework (FRUIT):
* [FRUIT](https://sourceforge.net/projects/fortranxunit/), downloaded by Makefile
* [FRUITPy](https://github.com/acroucher/FRUITPy) (Python interface to the FRUIT library)

## Build and run the solutions

    make runall

## Build and run the unit tests

    ./fruitpy.sh

## Build with optimized settings and run the benchmark

    make clean && make DEBUG= runbenchmark
