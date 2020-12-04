# Analyzing the 2020 Tour de France
Author: Gian Carlo Di-Luvi \
STAT 538 final project \
UBC Statistics \

[Final version](https://github.com/GiankDiluvi/tdf-2020/blob/main/doc/main.pdf)

## Description


The Tour de France is the most prestigious cycling race in the world. This year, Tadej Pogačar won the 2020 Tour after surprisingly beating favorite Primož Roglič—who had led the Tour for 11 stages—in stage 20, the last possible one. But was Pogačar's victory something we should have seen coming? In this report, we propose a zero-inflated model based on the Tweedie distribution to study how unlikely, or not, was Pogačar's victory. Specifically, we model the time difference of each rider to the race leader and train the model on data from the first 19 stages of the Tour. We then predict the results of stage 20—in which Pogačar won the Tour—finding that although the model predicted Roglič to be the Tour's winner, Pogačar had a 23\% estimated probability of winning as well.


## Directory roadmap
Each directory includes its own README file. In general:
* `data` contains the data set used for the analysis
* `doc` includes the written portions of the project: main, sections, appendices, and project outline tex/pdfs/aux files.
* `misc` contains miscelaneous files.
* `src` includes the R code developed for the project.
