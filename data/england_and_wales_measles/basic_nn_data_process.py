import pandas as pd

# Read the Parquet file
dftrain = pd.read_parquet('k1_train.gzip', engine='pyarrow')  # or engine='fastparquet'
dftest = pd.read_parquet('k1_test.gzip', engine='pyarrow')  # or engine='fastparquet'
df = pd.concat([dftrain, dftest])

max_population = df.groupby('city')['pop'].max().reset_index()
top_cities = max_population.sort_values(by='pop', ascending=False).head(10)
filtered_df = df[df['city'].isin(top_cities['city'])]

final_df = filtered_df.drop(['susc', 'pop', 'births', 'nearest_big_city', 'nearest_big_city_distances_unscaled', 'nbc_cases'], axis = 1)

for i in final_df.columns:
    print(i)

words = ['susc', 'london', 'birmingham', 'liverpool', 'manchester', 
         'sheffield', 'leeds', 'bristol', '_nc_1', '_nc_2', '_nc_3','_nc_4',
         '_nc_5','_nc_6','_nc_7','_nc_8', '_nc_9','_nc_10', 'nearest_1_city', 'nearest_2_city',
         'nearest_3_city', 'nearest_4_city', 'nearest_5_city', 'nearest_6_city', 
         'nearest_7_city', 'nearest_8_city', 'nearest_9_city', 'nearest_10_city']




columns_to_drop = [col for col in final_df.columns if any(word in col for word in words)]
df_dropped = final_df.drop(columns=columns_to_drop)

for i in df_dropped.columns:
    print(i)



df_dropped.to_parquet('uk_measles_10.parquet', engine='pyarrow', compression='snappy')


