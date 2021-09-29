# Setting up and designing an online blocking experiment with `R shiny`

As part of my PhD dissertation, I fielded an online framing experiment with the marketplace provider Lucid. The most convenient survey questionnaire design provider, Qualtrics, was not an option as the experiment depended on blocking to assign respondents to treatment group. Qualtrics does not offer such a feature. With the setup provided by the R package [ShinyPsych](https://github.com/mdsteiner/ShinyPsych). I instead opted to set up and design the experiment with R shiny. This repository contains the code to do so. Note: This is not an R package designed for generic use. It is merely an adaptation of ShinyPsych to work with more complex R code that was needed for my research purposes. I am making the code publicy available to potentially inspire others who wish to use R shiny for their survey experiments.

+++ Explain .an and .op (ANES/OP)

# Required setup

* `R` and `RStudio`
* `R` packages:
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

The code is designed like any `shiny` app. The specific app layout is explained [here](https://rpubs.com/msteiner/ShinyPsych_SurveyTutorial). While my code makes several adjustments to the package template, the basic structure remains the same.

Questions are created and stored in `.txt` files. An excellent detailed explanation of the inner workings is provided [here](https://rpubs.com/msteiner/ShinyPsych_TextfileTutorial). The `.txt` files contain the actual questions as well as the type of question (e.g. user input or radio buttons), the response choices (e.g. 5 different radio buttons), the response names (e.g. 5 different races) and whether the question order should be randomized, among several other things. While it is possible to set up and edit these parameters directly in `.txt` files, the potential for errors is high as these files are space-sensitive. It is recommended to carry out edits in `.csv` files and subsequently transform these to `.txt`. The code to do so 

In order to upload/download data to/from Dropbox, you need a Dropbox token named `droptoken.rds` in the `/survey_experiment` folder. You can create a Dropbox token with instructions from [rdrop2](https://github.com/karthik/rdrop2). The code to do so is included in `edit_package.R`. Since a Dropbox token guarantees anyone with it access to the Dropbox account, my personal token is not included in the folder. 
`edit_package.R` also includes several other `rdrop2` functions to analyze the eventual data, such as `drop_read_csv()`, `drop_dir()`, `drop_is_folder()`, and `drop_download()`. The latter is also used in the app itself, together with `drop_upload()`, to upload and download data to and from Dropbox. 
The code requires specified folders to communicate with Dropbox. These are: `alldata.op`, `alldata.an`, `seqblock.op`, and `seqblock.an`. The first two store the `.csv` files. The latter two store the `.Rdata` files that result from blocking. All folder names are arbitrary respective to substantive use.

# Content of the repository

## `/blocking_testing_dropbox`

A supplemental folder where I tested the interaction between the blocking algorithm and Dropbox. It is easiest to test this outside of `shiny`. This folder is not required for the functioning of the app.

## `edit_package.R`

+++ explain the details

## `/previous_working_versions`

A supplemental folder that contains previous working versions of the app. I developed the overall structure in the spring of 2019 and then extensively modified the code to reflect dissertation developments in late 2020, immediately before the survey was fielded. This folder is not required for the functioning of the app.

## `/questions`

A supplemental folder that contains all the survey questions, including instructions and treatment, in `.csv` and `.txt.` form. Treatment question are in a subfolder for organization reasons. While this folder is not required for the functioning of the app, it is tremendously useful for modifications and edits. As mentioned above, it is rather risky to edit the questions directly in `.txt` form due to the high potential for errors. It is much easier to do so in `.csv` form. This folder thus keeps both versions. I highly recommend keeping this folder outside of the actual app to avoid messing up previously working `.txt` files. Copying the files into the app is easily set up directly in `R` (see `edit_package.R`).

In my experiment, I use standard questions on demographics, party ID and ideology, as well as questions to assess respondents' levels of morality and self-interest and five treatment questions for each issue (healthcare and environment).

## `/survey_experiment`

This folder contains the actual app. All folders and files outside of this folder are purely for supplemental reasons. Respondents will only see content that is in this folder. `app.R` represents the `shiny` app. `\questions`, identical to above, contains all questions and instructions in `.txt` files. `/rsconnect` is created automatically when the app is deployed on [shinyapps.io](https://www.shinyapps.io). `sessionsInfo.txt` lists the OS, R version as well as all packages at the time of usage. `/www` contains external files to be shown in the questionnaire. In my case, these are my university logos.

+++ explain the details of `app.R`

# Contributing

+++ take contributing section from eagledown. Add that anyone should reach out if they want to use/ask anything (since this is probably all super confusing).






