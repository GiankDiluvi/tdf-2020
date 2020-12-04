# Code

Code to scrape the data set, tidy it, generate the figures included in the report (and more!) and fit the compound Poisson-Gamma models is in this directory.

### Directory roadmap

- `aux_data` contains auxiliary data files - that subdirectory has its own README explaining where each data set was obtained from
- `data_scrape.py` is the Python file to scrape the data from the Tour de France web site
- `tidy-data.R` combines the scraped data and data sets from the `aux_data` subdirectory into a single data set, creating relevant variables along the way
- `eda.R` creates visualizations based on the final data set
- `modeling.R` fits the two Tweedie regression models, predicts the outcome of stage 20, and creates model visualizations
- `tdf-analysis.ipynb` fits the Tweedie regression model with log response function in Python
