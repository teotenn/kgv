# kgv
R package for basic statistics for environmental sciences

# Synopsis

The package kgv (Katedra geoenvironmentálních věd) was created with the purpose of automatizing routinary statistical 
analysis performed for environmental geosciences in R. Thus, most of the functions here are based on basic R commands.

As the package is for environmental sciences, some functions for chemical calculations have been added, such a molarity
(Tt.mol) and ppm (Tt.ppm). However, the original function of the package was to automatize routinary statistical analysis,
as well as plots and tables useful for publications, based on ANOVA. Therefore, the package can be useful for any researcher,
even with little knowledge of R, to make easier and practical the repetitive creations of ANOVA and some of its derivates.
Critics, commentaries, ideas and opinions about new functions related to this topic are welcome to improve the package, as
well as collaboration of code.


# Functions and data
All the functions and data can be used as in any R package. See help in R for details. Here we present a general description
of all the functions and data included in the package.

- **Tt.aov** Performs 1 and 2-ways ANOVA, using the functions `aov` and `anova`. The function returnsan object of class list with a table of mean values, standard deviation and size of sample per treatment. If p>0.05, Tukey test is also performed.
- **Tt.barplot** Creates a ggplot object of a barplot with error lines. The data frame obtained from the function `Tt.aov$stats` can be used directly to create a plot showing mean and standard errors.
- **Tt.table** Writes a table with mean +/- standard deviation, pasting them together as characters, separated by default by +/- symbol, or your input symbol. Here again, results obtained from the function `Tt.aov$stats` can be used directly to create a table. This table is useful to be exported directly as csv file for its eddition for publications. You can also export it to LaTeX with the right tools and the results are pretty much ready to be used.
- **Tt.mol** A chemical formula is provided in a specific format in order to calculate the weight of solute necesary to produce a solution with given molarity.
- **Tt.spectra** The function uses basic R plot for plotting a spectra. If `elements.list` is provided, it will include in the plot labels of the chemical elements in the list, based on the energy (KeV) table of the elements for EDS analysis.
- **Tt.read.abs** Creates the linear model, based on the absorbance of samples with known concentrations. If added, the function can also calculate (predict) the concentrations of further samples based on the formula of the generated model.
- **Tt.ppm** Calculates the proportion of element contained in a solid, expressed as mass units of element per mass units of sample (g, ug, mg, Kg), after being processed through a solution, particularly useful after reading absorbance, AA, ICP, etc. The desired units can be chosen in the parameters to adapt it as wished (ppm, ppb or any other of the form g/Kg).
- `data(elements)` Data frame containing information of the periodic table of elements,based on the package `PeriodicTable`. The only addition to the original data frame is the last two columns (EDS and EDS2) containing the energy (KeV) table for EDS analysis.
- `data(compost)` Object of class `data.frame`, with 40 rows and 7 columns, containing 2 independent and 5 dependent variables. Useful for examples of 1 and 2-ways anova.

# License
This package is free and open source software, licensed under GPL.
