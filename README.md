<<<<<<< HEAD
# LittleApp: A package for building statistics demonstrations

Little Apps are interactive statistics demonstrations. They are intended to have a highly consistent interface and to be fundamentally oriented around  displays of data, typically point plots. Statistics and models are displayed, as required, as annotations layered on the point plot.

`LittleApps` is an R package that supports writing Little Apps, which are themselves written in `shiny`. The `LittleApps` package provides functions for creating the user interface and "reactives" that handle common tasks and displays.

At the core of a generic Little App is a point-plot frame defined by a *response* variable and an *explanatory* variable. Many Little Apps incorporate  one or two covariates which might be displayed as color or faceting.

Below the  core plot is a set of tabs. Typically, these include:

- *Explain*: An explanation of how to use the Little App
=======
# LittleApp

This repository contains a proposed re-factoring of the Little Apps, based on feedback from year 2 of the StatPREP project. 

* Uses shiny dashboard for a more consistent UI and more compact controls.
* Extends the range of datasets that can be used.
* Modifies the resampling/randomizaton paradigm so that:
    - the sample is always genuine data.
    - the randomization trials are stored in one array that can be accessed directly for graphing or statistical calculations.
    
Rather than using a new branch in the original LittleApp repository, this is an entirely new repository. That makes it easier to look at the original for adopting bits from it to this new re-factoring.
    
    
Claus Wilke, HOPS, graphics from his https://github.com/wilkelab/ungeviz
>>>>>>> fdc9b828a451d87c064be8345ed0780f8b4b5af1
