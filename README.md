# littleapps2

This repository contains a proposed re-factoring of the Little Apps, based on feedback from year 2 of the StatPREP project. 

* Uses shiny dashboard for a more consistent UI and more compact controls.
* Extends the range of datasets that can be used.
* Modifies the resampling/randomizaton paradigm so that:
    - the sample is always genuine data.
    - the randomization trials are stored in one array that can be accessed directly for graphing or statistical calculations.
    
Rather than using a new branch in the original LittleApps repository, this is an entirely new repository. That makes it easier to look at the original for adopting bits from it to this new re-factoring.
    