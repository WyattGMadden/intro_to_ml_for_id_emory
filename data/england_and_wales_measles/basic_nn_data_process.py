import pandas as pd

# Read the Parquet file
dftrain = pd.read_parquet('k1_train.gzip', engine='pyarrow')  # or engine='fastparquet'
dftest = pd.read_parquet('k1_test.gzip', engine='pyarrow')  # or engine='fastparquet'
df = pd.concat([dftrain, dftest])

max_population = df.groupby('city')['pop'].max().reset_index()
top_cities = max_population.sort_values(by='pop', ascending=False).head(30)
filtered_df = df[df['city'].isin(top_cities['city'])]


filtered_df.to_parquet('uk_measles_30_cities.parquet', engine='pyarrow', compression='snappy')


# Display the DataFrame
print(filtered_df.head())



