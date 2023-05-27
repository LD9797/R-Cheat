import pandas as pd

# Load the CSV file
df = pd.read_csv('RandomizedExperiments.csv')


# Create a function to get normalized minutes from "Time(m)" column
def round_minutes(x):
    return round(float(x), 2)


# Apply function to get normalized minutes
df['Time(m)Normalized'] = df['Time(m)Normalized'].apply(round_minutes)

# Remove the specified columns
df = df.drop(columns=['Run', 'Time(m)Raw', 'Min', 'Seconds'])

# Merge 'Accelerator' and 'Integrator' into a new column 'Acc-Int'
df['Acc-Int'] = df['Accelerator'] + "-" + df['Integrator']

# Remove the 'Accelerator' and 'Integrator' columns
df = df.drop(columns=['Accelerator', 'Integrator'])

# Remove the specified substrings from the 'Combination' column
df['Combination'] = df['Combination'].replace({'_bvh': '', '_volpath': '', '_kdtree': '', '_path': ''}, regex=True)

# Remove rows with missing 'Time(m)Normalized' values
df = df.dropna(subset=['Time(m)Normalized'])

# Rename 'Combination' to 'Scene' and 'Time(m)Normalized' to 'Time'
df = df.rename(columns={'Combination': 'Scene', 'Time(m)Normalized': 'Time'})

# Write the dataframe back to CSV
df.to_csv('updated_file.csv', index=False)
