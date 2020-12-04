# scrape Tour de France data from the Tour's website
# author: Gian Carlo Diluvi

import pandas as pd
import numpy as np


url='https://www.letour.fr/en/rankings/stage-'

links = np.array([])
for i in np.arange(21):
    links = np.append(links, url + str(i+1))

final = []

for link in links:
    result = pd.read_html(link)

    #result['stage_results_id'] = np.repeat('stage-' + str(i+1), len(result[0][0:].index))
    header = result[0][0:0]
    final.append(result[0][0:])
    #print(type(result), type(result[0][0:]), type(final))

df = pd.concat(final, sort=False)
df.drop_duplicates()
df.index = pd.RangeIndex(len(df.index))

df['year'] = 2020*np.ones(len(df.index))
df['edition'] = 107*np.ones(len(df.index))
df[['edition', 'year', 'Rank', 'Rider', 'Team', 'Rider No.']]

print(df.head())
#df.to_csv('aux_data/scraped.csv', index = False) # original
df.to_csv('aux_data/scraped_v2.csv', index = False) # TdF webpage will change its content at some point, so change name to not overwrite original scraped data
