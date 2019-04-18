# LittleApp: A package for building statistics demonstrations

Little Apps are interactive statistics demonstrations. They are intended to have a highly consistent interface and to be fundamentally oriented around  displays of data, typically point plots. Statistics and models are displayed, as required, as annotations layered on the point plot.

`LittleApps` is an R package that supports writing Little Apps, which are themselves written in `shiny`. The `LittleApps` package provides functions for creating the user interface and "reactives" that handle common tasks and displays.

At the core of a generic Little App is a point-plot frame defined by a *response* variable and an *explanatory* variable. Many Little Apps incorporate  one or two covariates which might be displayed as color or faceting.

Below the  core plot is a set of tabs. Typically, these include:

- *Explain*: An explanation of how to use the Little App
