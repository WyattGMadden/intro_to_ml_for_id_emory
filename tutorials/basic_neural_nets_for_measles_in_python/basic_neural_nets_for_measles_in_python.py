# -*- coding: utf-8 -*-
"""basic_neural_nets_for_measles_in_python.ipynb

Automatically generated by Colab.

Original file is located at
    https://colab.research.google.com/drive/1OtzqD79_aWC2M-pW_EJJiv0cq17LSMhS

## Enabling and testing the GPU

First, you'll need to enable GPUs for the notebook:

- Navigate to Edit→Notebook Settings
- select GPU from the Hardware Accelerator drop-down

Next, we'll confirm that we can connect to the GPU with tensorflow:
"""

import torch

if not torch.cuda.is_available():
    raise SystemError('GPU device not found')
print(f'Found GPU at: {torch.cuda.get_device_name(torch.cuda.current_device())}')

"""If you are having issues connecting to a GPU you can ignore this and continue with the tutorial. The model will run on 'cpu' and will likely take longer to fit.

# Notes on Python

Here is a basic refresher on python syntax. Feel free to skip this section if you are already comfortable with python.

## Basic Python

### Variable Types
"""

integer_var = 10
float_var = 5.5
string_var = "Hello, World!"
boolean_var = True

"""### Conditional Statements"""

age = 17

if age < 18:
    print("You are a minor.")
elif age >= 18 and age < 65:
    print("You are an adult.")
else:
    print("You are a senior.")

"""### Loops"""

# for loop
for i in range(5):  # range(5) generates numbers from 0 to 4
    print(i)

# while loop
counter = 0
while counter < 5:
    print(counter)
    counter += 1

"""### Functions"""

def greet(name):
    print("Hello " + name)

greet("John")
greet("Jane")

"""### Lists

"""

fruits = ["apple", "banana", "orange"]
fruits
fruits.append("grape")
fruits
fruits.remove("banana")
fruits

fruits[0]
fruits[0] = "pineapple"
fruits

fruits

"""## Python Libraries

Python libraries are like R packages. They often have extensive documentation online. We will use the following python packages in this tutorial:

- numpy: math and working with matrices
- pandas: cleaning/wrangling data frames
- sklearn: machine learning toolkit
- matplotlib: plotting
- seaborn: more plotting
- torch: neural networks, backprop optimization

# Read In Data
In this tutorial we will forecast measles incidence using neural networks in PyTorch. We will employ the seminal England and Wales pre-vaccination bi-weekly measles dataset (a description of which can be found [here](https://doi.org/10.1371/journal.pcbi.1010251)).
We will work with the ten most populous cities from 1949-1965.
"""

import pandas as pd
import requests
import io

# URL of the raw Parquet file on GitHub
url = 'https://raw.githubusercontent.com/WyattGMadden/intro_to_ml_for_id_emory/main/data/england_and_wales_measles/uk_measles_10.parquet'

# Make a request to get the Parquet file and read in the dataset as a pandas dataframe
r = requests.get(url)
data = io.BytesIO(r.content)
measles = pd.read_parquet(data)

"""# Explore Data

Lets explore the data with some plots and summaries. Note that the data includes time, city, and cases (this will be target), and standardized case lags, nearest-big-city case lags, population lags, birth lags, and nearest-big-city distances (these will be features). Lags include one-step (bi-week) lags through 130-step lags. Since we will include all lags as features, we will be predicting one-step-ahead measles using the previous 5-years of data for each time step.
"""

print(measles)

# example of method
# first n rows of data frame
measles.head()

# example of attribute
# list of data frame columns
measles.columns

for i in measles.columns:
  print(i)

# get second column
measles.cases

# get value in second row, first column
measles.iloc[1, 0]

"""Let's plot all cases over time, with separate lines for each city."""

import matplotlib.pyplot as plt
import seaborn as sns

plt.figure(figsize=(10, 6))
sns.lineplot(data=measles, x='time', y='cases', hue='city', style='city', dashes=False, alpha=0.5)
plt.title('Measles Cases Over Time by City')
plt.legend(title='City', bbox_to_anchor=(1.05, 1), loc=2, fontsize='x-small')
plt.tight_layout()
plt.show()

"""## Exercise

- Make a plot of just the London cases over time.
"""

london_measles = measles[measles["city"] == "London"]

# plot here:

"""# Split Data Into Train/Test

Next we split the dataset into a training and testing set. We will train on cases prior to 1960 and test on cases after and including 1960. We also standardize within-city (separate means and standard deviations for each city) and transform the target cases with f(x) = log(x + 1)

"""

import sklearn
import numpy as np
from sklearn.preprocessing import StandardScaler

# Assume 'measles' DataFrame is already defined with 'time', 'city', and 'cases'
measles['cases_logplus1'] = np.log(measles['cases'] + 1)

# Initialize a dictionary to store scalers for each city
scalers = {}

# Apply scaling within each city
for city in measles['city'].unique():
    scaler = StandardScaler()
    mask = measles['city'] == city  # Create a mask for each city
    measles.loc[mask, 'cases_logplus1_standardized'] = scaler.fit_transform(measles.loc[mask, ['cases_logplus1']])
    scalers[city] = scaler  # Store the scaler for potential future use

measles_train = measles[measles["time"] < 60].copy()
measles_test = measles[measles["time"] >= 60].copy()

# Drop non-feature columns to create feature sets
measles_train_features = measles_train.drop(["time", "city", "cases", "cases_logplus1"], axis=1).to_numpy()
measles_test_features = measles_test.drop(["time", "city", "cases", "cases_logplus1"], axis=1).to_numpy()

# Extract targets
measles_train_target = measles_train['cases_logplus1_standardized'].to_numpy().reshape(-1, 1)
measles_test_target = measles_test['cases_logplus1_standardized'].to_numpy().reshape(-1, 1)

# inspect training feature dimensions
measles_train_features.shape

# inspect test feature dimensoins
measles_test_features.shape

# inspect all targets
measles_train_target

"""# Pytorch Neural Net

Now we are ready to train our neural network.

## Design and Run Model

We use the Dataset class (included in PyTorch) to prepare the data for the model. This includes converting the numpy arrays to tensors and writing some methods (\_\_getitem\_\_, \_\_len\_\_) that are used in the fitting process.

We push the Dataset instance through the Dataloader() class to create an iterable dataset that is easy to batch over in the fitting process.
"""

import torch
from torch import nn
from torch.utils.data import Dataset, DataLoader


# data is prepared for pytorch by using the Dataset class
# this allows the dataset to be easily batched and iterated through
class Data(Dataset):
    def __init__(self, X, y):
        self.X = torch.tensor(X).float()
        self.y = torch.from_numpy(y).float()
        self.len = self.X.shape[0]

    def __getitem__(self, index):
        return self.X[index], self.y[index]

    def __len__(self):
        return self.len


# create Dataset/DataLoader class instances from measles data
train = Data(measles_train_features, measles_train_target)
train_loader = DataLoader(train, batch_size = 64, shuffle = True)

test = Data(measles_test_features, measles_test_target)

# inspect the first batch of the measles training data dataloader instance
for i, (features, targets) in enumerate(train_loader):
  print(features.shape, targets.shape)
  break;

"""Next we design and instantiate the model architecture. We use a feed-forward neural network with one hidden layer and ReLu activation functions."""

# all pytorch neural nets have the same basic form (class)
class NeuralNetwork(nn.Module):
    def __init__(self, input_dim, hidden_dim, output_dim):
        super(NeuralNetwork, self).__init__()
        #self.flatten = nn.Flatten()
        self.linear_relu_stack = nn.Sequential(
            nn.Linear(input_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, hidden_dim),
            nn.ReLU(),
            nn.Linear(hidden_dim, output_dim),
            )
    def forward(self, x):
        #x = self.flatten(x)
        return self.linear_relu_stack(x)


# create the specific model instance that will be used to fit the neural net on the heart data
model = NeuralNetwork(
    input_dim = measles_train_features.shape[1],
    hidden_dim = int(measles_train_features.shape[0] * (2/3)),
    output_dim = measles_train_target.shape[1]
    )

model

"""Finally we are ready to fit the model."""

# choose the loss function
loss_fn = nn.MSELoss()

# choose the optimization routine
optimizer = torch.optim.Adam(model.parameters(), lr = 0.01)

# choose the number of times the model runs through all the measles data
num_epochs = 20

# create empty lists to store the train/test loss at each epoch
train_loss_values = []
test_loss_values = []

# tell pytorch that we are starting the training process
model.train()


# fit the model epoch * batch_size number of times.
# Each time updates the parameters
for epoch in range(num_epochs):
    print("Starting Epoch: " + str(epoch))
    train_loss = 0
    i = 0
    for features, target in train_loader:

        # keep track of batch number
        i += 1

        # zero the parameter gradients
        optimizer.zero_grad()

        # run the model on batched test features
        pred = model(features)

        # calculate loss
        loss = loss_fn(pred, target)

        # add loss to training_loss for accounting purposes
        train_loss += loss.item()

        # backpropogate loss
        loss.backward()

        # update parameters
        optimizer.step()

        print("Batch number: " + str(i))

    # save train/test set performance, append to lists
    with torch.no_grad():
      train_loss_values.append(train_loss / i)
      test_pred = model(test.X)
      test_loss = loss_fn(test_pred, test.y)
      test_loss_values.append(test_loss.item())

    # print the training loss to monitor during fitting
    print("Finishing Epoch: " + str(epoch))
    print("Train Loss: " + str(train_loss_values[epoch]))
    print("Test Loss: " + str(test_loss_values[epoch]))

print("Training Complete")

"""## Evaluate Model

First let's plot the train/test loss over fit iterations. The test loss (orange) and train loss (blue) both decrease over epochs. While there is some leveling-off, it is likely that training the model for additional epochs would result in improved test-set performance.
"""

import matplotlib.pyplot as plt
import numpy as np
plt.plot(range(0, len(train_loss_values)), np.log(train_loss_values), color = "blue")
plt.plot(range(0, len(train_loss_values)), np.log(test_loss_values), color = "orange")

"""Next we convert the predictions to numpy arrays for ease of metrics/plotting."""

# tell pytorch that we are no longer fitting the model
# parameters will no longer update
model.eval()

# get model predictions for train/test/validation data sets
train_preds = model(train.X)
test_preds = model(test.X)

# convert tensor of training predictions to numpy array
train_preds_np = train_preds.detach().numpy().flatten()
# convert tensor of training targets to numpy array
train_np = train.y.detach().numpy().flatten()

# convert tensor of test predictions to numpy array
test_preds_np = test_preds.detach().numpy().flatten()
# convert tensor of test targets to numpy array
test_np = test.y.detach().numpy().flatten()

"""The MSE appears low, and is slightly worse for the training set."""

train_mse = np.mean((train_preds_np - train_np)**2)
test_mse = np.mean((test_preds_np - test_np)**2)
print("Train MSE: " + str(train_mse))
print("Test MSE: " + str(test_mse))

"""Let's plot (standardized) predictions and truth for all data points. Here each city is stacked on-end."""

# plot test predictions and test target over time (all cities on-end)
#
plt.figure(figsize=(10, 6))
plt.plot(test_np, label='Actual')
plt.plot(test_preds_np, label='Predicted')
plt.legend()

"""Next let's unscale the data and clean up the final data set."""

# include predictions in original dataset
measles_train["preds_logplus1_scaled"] = train_preds_np
measles_test["preds_logplus1_scaled"] = test_preds_np

# Function to unscale predictions by city
def unscale_by_city(data):
    for name, scaler in scalers.items():
        if name in data['city'].unique():
            scaled_data = data.loc[data['city'] == name, 'preds_logplus1_scaled'].values.reshape(-1, 1)
            unscaled_data = scaler.inverse_transform(scaled_data)
            data.loc[data['city'] == name, 'preds_logplus1_unscaled'] = unscaled_data

# Unscale predictions
unscale_by_city(measles_train)
unscale_by_city(measles_test)

# Reverse log transformation
measles_train["preds"] = np.exp(measles_train["preds_logplus1_unscaled"]) - 1
measles_test["preds"] = np.exp(measles_test["preds_logplus1_unscaled"]) - 1

# Tag data as train or test
measles_train["test_train"] = "train"
measles_test["test_train"] = "test"

# Combine back into a single dataframe
measles_preds = pd.concat([measles_train, measles_test])

"""Now we can assess predictions on the original scale. Let's first just plot London predictions over true case values."""

# join origin cases on measles_preds
measles_preds["true"] = measles_preds["cases"]


#measles_train["true"] = train_np
#measles_test["true"] = test_np

measles_long = pd.melt(
    measles_preds,
    id_vars=['time', 'city', 'test_train'],
    value_vars=['preds', 'true'],
    var_name='case_type',
    value_name='Cases')
measles_london_long = measles_long[measles_long["city"] == "London"]
plt.figure(figsize=(10, 6))


import seaborn as sns
# Use Seaborn to plot
sns.lineplot(
    data=measles_london_long,
    x='time',
    y='Cases',
    hue='case_type',
    style='test_train',
    dashes=True,
    palette='tab10'
    )

plt.title('London Predicted vs Actual Cases Over Time')
plt.xlabel('Time')
plt.ylabel('Number of Cases')
plt.show()

"""The model appears to forecast one-step-ahead fairly well, though may do better with more epochs.

## Exercises
- Inspect the test/train predicted/true cases for a different city (other than London)
- Run the model for more epochs to see if performance improves.
- Experiment with different hyperparameters/architectures, and investigate change in performance. For example, you could change batch size, optimizer, learning rate (and other optimizer parameters), hidden layer dimension, number of hidden layers, activation function, etc..

Some useful links:
- https://pytorch.org/docs/stable/nn.html#non-linear-activations-weighted-sum-nonlinearity
- https://pytorch.org/docs/stable/optim.html

# Additional Resources

# Neural Networks/Deep Learning

- guide to Google Colab: https://colab.research.google.com/notebooks/welcome.ipynb#scrollTo=-Rh3-Vt9Nev9

- Fast.ai Deep Learning Course https://course19.fast.ai/index.html

- MNIST in pytorch: https://colab.research.google.com/github/omarsar/pytorch_notebooks/blob/master/pytorch_quick_start.ipynb

- Andrej Karpathy's (OpenAI, former director AI at Tesla) Neural Networks: Zero to Hero Youtube course https://www.youtube.com/watch?v=VMj-3S1tku0&list=PLAqhIrjkxbuWI23v9cThsA9GvCAUhRvKZ&index=1

- UvA Deep Learning Tutorials https://uvadlc-notebooks.readthedocs.io/en/latest/index.html

- Dive Into Deep Learning (applied deep learning textbook/examples) https://d2l.ai/

# General Python

- https://docs.python.org/3/tutorial/
"""