import numpy as np
import pandas as pd
import ast
from collections import defaultdict

ted_data = pd.read_csv('c:\\Users\\Gurpreet\\Documents\\Coding practices\\Viz (R)\\ted_talk_clean.csv')

ted_data.keys()

rating_names = set()

for index, row in ted_data.iterrows():
    rating = ast.literal_eval(row['ratings'])
    for item in rating:
        rating_names.add(item['name'])
    
print (rating_names)



rating_data = defaultdict(list)
for index, row in ted_data.iterrows():
    rating = ast.literal_eval(row['ratings'])
    rating_data['talk_id'].append(row['talk_id'])
    names = set()
    for item in rating:
        rating_data[item['name']].append(item['count'])
        names.add(item['name'])

rating_data = pd.DataFrame(rating_data)
rating_data.head()
rating_data.shape

#rating_data.to_csv('c:\\Users\\Gurpreet\\Documents\\Coding practices\\Viz (R)\\rating.csv')

rating_values = rating_data.loc[:, (rating_data.columns != 'total') &  (rating_data.columns !='Talk_ID')] 
rating_data['main_rating'] = rating_values.apply(np.argmax, axis = 1)

rating_data['main_rating'].head()
rating_data.to_csv('c:\\Users\\Gurpreet\\Documents\\Coding practices\\Viz (R)\\rating.csv',index=False)
