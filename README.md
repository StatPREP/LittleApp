# LittleApp: A package for building statistics demonstrations

Little Apps are interactive statistics demonstrations. They are intended to have a highly consistent interface and to be fundamentally oriented around  displays of data, typically point plots. Statistics and models are displayed, as required, as annotations layered on the point plot.

`LittleApps` is an R package that supports writing Little Apps, which are themselves written in `shiny`. The `LittleApps` package provides functions for creating the user interface and "reactives" that handle common tasks and displays.

At the core of a generic Little App is a point-plot frame defined by a *response* variable and an *explanatory* variable. Many Little Apps incorporate  one or two covariates which might be displayed as color or faceting.

Below the  core plot is a set of tabs. Typically, these include:

- *Explain*: An explanation of how to use the Little App
- *Codebook*: Documentation for the data frame currently displayed in the graphic.
- *Statistics*: A tabular or text presentation relevant to the statistics being demonstrated, e.g. a regression table or a t-test report.
- *R commands*: A brief introduction to the R commands underlying the graphics and/or statistics displayed by the app.

A control bar lies to the left of the core plot and the tabs. The controls are divided into  several blocks, some of which are specific to the topic of the individual app. There are two control blocks that are found in (almost?) every app:

1. *Data source* with  which the user selects a data frame and assigns variables to the various roles (e.g. response, explanatory, covariate) in the app.
2. *Sample* which allows the user to set the sample size $n$ and to refresh the graphics and statistics displays with a new sample. In some apps, typically when the explanatory variable is categorical, there is an option to stratify the sampling. Doing so will result in $n$ points at each categorical level. (When the data aren't sufficient to provide this for each  level, resampling is done.) 


----------

Some of the bootstrapping displays make use of Claus Wilke's `ungeviz` package available at <https://github.com/wilkelab/ungeviz>. 
