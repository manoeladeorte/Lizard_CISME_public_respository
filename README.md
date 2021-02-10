# Introduction

This is a public repository containing all of the data and code necessary to reproduce the results in:

**Unexpected contribution of communities colonizing dead coral substrate to the net calcification of coral reefs**

All data analysis and visualization were performed in R (v. 4.0.3). A list of necessary packages can be found in `scripts/initialize_workspace.R`.

# Details

All data associated with this project can be found in the `data` folder. 

All scripts to process, analyze, and visualize the data can be found in the `scripts` folder.

All plot outputs can be found in `outputs/figures`.

To reproduce the results in a given figure, simply run the script to build that figure. For example, to generate the results in Figure 2 and build the figure, simply run `source("build_figure_2.R")`. 

To reproduce the results of the tests of statistical significance, simply execute the script `perform_hypothesis_test.R` in the `scripts` folder. Executing this script produces a summary file of all statistical significance test results called `outputs/significance_tests/test_outputs.txt`.

All dependencies needed to execute a script are listed at the top of the script. 

# Contact

Please contact me with any questions or comments. 

Happy exploring.

[Manoela](https://www.manoeladeorte.com/)
