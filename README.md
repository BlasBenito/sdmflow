**Looking for collaborators**: I would like this R package to become a collaborative effort, and I am looking for collaborators and/or testers (everyone is welcome!). The idea would be to couple the package development it with at least two collaborative papers: a software note, and an application case highlighting the importance of variable selection and modelling paradigm when building species distribution models. If you are interested, please send me an email to blasbenito at gmail.com or a message on twitter to @blasbenito

# sdmflow

**Work in progress!!**

This R package intends to facilitate the design and execution of scientific workflows oriented to model species distributions over space and past-time. It particularly focuses on:

+ Providing a consistent modeling grammar.
+ Reducing environment cluttering by providing *ad-hoc* object classes to store results.
+ Fitting models according to the paradigm "use versus availability".
+ Providing a didactic documentation in the form of vignettes and function help files.

## Modeling grammar

It particularly intends to facilitate the design of SDM workflows by providing a consistent modeling grammar, as easy to remember as possible, in order to reduce the cognitive load produced by packages with large numbers of functions. This grammar is based on the idea that an SDM workflow is composed by a limited set of conceptual steps:

*Note: most of the functions mentioned below are still a work in progress.*

+ 1. **V**ariable preparation, performed by functions named **v_{action}**. For example, **v_match** would match the coordinate reference system, extension, resolution, and mask, of a set of rasters from different sources stored in the same folder, or **v_read**, which would read those rasters into a stack or a brick.

+ 2. **O**currence preparation, done with functions named **o_{action}**, such as **o_reproject**, which would reproject the presence data to the same reference system of the raster data, or **o_prepare**, which would prepare the training data files required to fit SDMs from the presence data and the raster variables.

+ 3. **S**election of predictors, denoted by functions named **s_{whatever}**, such as **s_lower_cor** or **s_lower_vif**, which help to reduce multicollinearity in a training dataset, or **s_auto**, which selects variables automatically based on an estimate of their potential effect.

+ 4. **M**odeling stage, performed with functions named **m_{whatever}**, such as **m_fit** to fit different models, **m_performance** to evaluate models, **m_importance** to assess variable importance, **m_project** to project SDMs into different times or places, or **m_ensemble** and **m_stack** to ensemble different models of the same species or stack together models of different species.

+ 5. **R**eporting stage, intended to facilitate the storage and communication of modeling results, and peformed with functions named **r_{whatever}**, such as **r_metadata** to generate metadata documentation for each outcome of a modeling workflow, or **r_report** to generate an Rmarkdown template reporting the outcomes of a particular model.

Each stage will be as well represented by a single function that can perform at once the most important steps of each stage, so a complete modeling workflow could be written as follows, once the pertinent parameters are filled:

```
v_auto(...) %>%     #variable preparation
o_auto(...) %>%     #occurrence preparation
s_auto(...) %>%     #automatic variable selection
m_auto(...) %>%     #automatic modeling and evaluation
r_auto(...)         #report generation
```

## Object classes

Another important target of "sdmflow" is to reduce the cognitive load produced by a cluttered environment. To solve this issue the package will rely on three main object classes (well, named lists with a particular structure):

+ **sdmflow.environment** stores raster data in 4D (*latitude*, *longitude*, *value*, *variable*) or 5D (the latter plus *time*)

+ **sdmflow.training** stores the training data frame along with other relevant objects resulting from variable screening (i.e. VIF results), the names of the selected variables, and the model formulas to fit models downstream.

+ **sdmflow.model** stores the results of a model, including the model object itself, assessment results, variable importance, raster projection of the model, etc.


## Modeling paradigm

The package **sdmflow** is based on the "use versus availability" modeling paradigm, which assumes that the presence records somehow reflect how a species uses the available habitat, which is represented by the *background data*. This modeling method gives higher habitat suitability to environmental values that are *rare* but *disproportionately used* by the species, following the idea that an accumulation of presence records over abundant environmental values can be the result of random processes, while the accumulation of presence records on rare environmental values is a clear signal of habitat selection.

The main advantage of this methods comes from its reliance on background data. Unlike absence or pseudo-absence data, background data does not have any interpretation problems, and simply represent an comprehensive sampling of the environmental conditions of the study area. This package also includes an option to work with "restricted background", generally taken from the area that is accessible to the species by dispersal.

## Comprehensive documentation

This package was born after years of teaching species distribution models for GBIF.es, and I would like its documentation and vignettes to be as comprehensive as possible, so the package can become a source of tools and knowledge at the same time.






