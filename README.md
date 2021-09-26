# Setting up and designing an online blocking experiment with R shiny

As part of my PhD dissertation, I fielded an online framing experiment with the marketplace provider Lucid. The most convenient survey questionnaire design provider, Qualtrics, was not an option as the experiment depended on blocking to assign respondents to treatment group. Qualtrics does not offer such a feature. With the setup provided by the R package [ShinyPsych](https://github.com/mdsteiner/ShinyPsych). I instead opted to set up and design the experiment with R shiny. This repository contains the code to do so. Note: This is not an R package designed for generic use. It is merely an adaptation of ShinyPsych to work with more complex R code that was needed for my research purposes. I am making the code publicy available to potentially inspire others who wish to use R shiny for their experiments.

# Required setup

* R and RStudio
* R packages:
    + `blockTools`
    + `stringr`
    + `ShinyPsych`
    + `shinyjs`
    + `plyr`
    + `rdrop2`
    + `rsconnect`
    + `shiny`
    + `here`
* A [shinyapps.io](https://www.shinyapps.io) account
* A [Dropbox](https://www.dropbox.com) account

# General structure

The code is designed like any `shiny` app. The specific app layout is explained [here](https://rpubs.com/msteiner/ShinyPsych_SurveyTutorial). While my code makes plenty of adjustments to the package template, the basic structure remains the same.

Questions are created and stored in `.txt` files. An excellent detailed explanation of the inner workings is provided [here](https://rpubs.com/msteiner/ShinyPsych_TextfileTutorial). A bit more here +++

In order to upload/download data to/from Dropbox, you need a Dropbox token named `droptoken.rds` in the `/survey_experiment` folder. You can create a Dropbox token with instructions from [rdrop2](https://github.com/karthik/rdrop2). The code to do so is included in `edit_package.R`. The code uses several function (+++) from the package to upload and download data to and from Dropbox. Since a Dropbox token guarantees anyone with it access to the Dropbox account, my personal token is not included in the folder. The code requires specified folders to communicate with Dropbox. These are: `alldata.op`, `alldata.an`, `seqblock.op`, and `seqblock.an`. The first two store the `.csv` files. The latter two store the `.Rdata` files that result from blocking.

+++ Explain .an and .op (ANES/OP)

# Content of the repository

## `/blocking_testing_dropbox`

This is simply a supplemental folder where I tested the interaction between the blocking algorithm and Dropbox. It is easiest to test this outside of `shiny`.

## `edit_package.R`

+++ explain the details

## `/previous_working_versions`

This folder contains previous working versions of the app. I developed the overall structure in the spring of 2019 and then extensively modified the code to reflect dissertation developments in late 2020, immediately before the survey was fielded.

## `/questions`

This folder contains all the survey questions, including instructions. There is no need to keep the questions in a subfolder, but I did so for organization reasons. I filed the experimental treatment questions in further subfolder on the same basis.

+++ what questions do I use

## `/survey_experiment`

This folder contains the actual app. All folders and files outside of this folder are purely for supplemental reasons. Respondents will only see content that is in this folder. `app.R` represents the `shiny` app. `\questions`, identical to above, contains all questions and instructions in `.txt` files. `/rsconnect` is created automatically when the app is deployed on [shinyapps.io](https://www.shinyapps.io). `sessionsInfo.txt` lists the OS, R version as well as all packages at the time of usage. `/www` contains external files to be shown in the questionnaire. In my case, these are my university (American university) logos.

+++ explain the details of `app.R`

# Contributing

+++ take contributing section from eagledown. Add that anyone should reach out if they want to use/ask anything (since this is probably all super confusing).






