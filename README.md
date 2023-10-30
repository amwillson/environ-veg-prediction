# environ-veg-prediction
Using historical vegetation-environment relationship to predict modern vegetation distribution across the Upper Midwest, USA.

Contributing authors: AM Willson, C Kowalski, J McLachlan

(Other contributing authors will be added as the project progresses.)

This repository contains analysis and writing for a manuscript led by AM Willson investigating the consequences of model overfitting for intepretation of the spatial extent of multiple ecosystem states.

**How to use:**

The repository is broken up into subdirectories: scirpts, data, paper, and info.

* **data**: contains raw and procecessed cliamte and vegetation data for the analysis (in separate subdirectories). ANY DATA FILES THAT YOU ARE REPLACING SHOULD BE MOVED TO A SEPARATE "DEPREDCATED" SUBDIRECTORY AND NOT DELETED.
* **info**: contains general notes, project timeline, contributions, and other information pertinent to working on the project. Any updates to your progress, criticisms of current work, etc. should be documented in this subdirectory through frequent commits to maintain a log of communication.
* **paper**: contains separate markdown documents for sections of the paper. Edits can be made by simply writing over text and submitting a pull request. Comments can be made by just writing your comment as a part of the document (as a NEW PARAGRAPH) and creating a pull request. For now, please tag comments (as opposed to edits) by using [COMMENT] at the beginning of the paragraph.
* **scripts**: contains all code for the entire analysis. Ideally, please keep scripts numbered in the order that they should be run. You may change the numbering of scripts (e.g., if you're adding a step), but you must commit all file name changes!! When writing code, assume anyone running the code is using the project feature so that the working directory is the base directory of the repository. Please keep your R up-to-date and note any packages you are using that are not up-to-date. ANY SCRIPTS YOU ARE COMPLETELY DELETING OR RE-WRITING SHOULD BE MOVED TO A "DEPRECATED" FOLDER AND NOT DELETED. Please update the README with a short description of any script you add to the analysis.

**data**

[Description of any data in this directory]

**info**

- Outline_30-10-23.Rmd: Initial outline produced during a brainstorming session between AW, JM, and CK

**paper**

[Description of any markdown documents making up the paper in this directory]

**scripts**

[Description of all code scripts used to produce the analysis]