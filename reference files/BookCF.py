
#Collaborative Filtering using Python
#http://files.grouplens.org/datasets/movielens/ml-100k.zip
import os
import numpy as np
import pandas as pd
os.chdir('C:\\Users\\christopher.brossman\\Documents\\Projects\\work\\Recommender System\\Anton-Practice')
path = "udata.csv" 
df = pd.read_csv(path) 
type(df)

df.head()
df.columns
df.shape
 
#create a variable n_users to find the total number of unique users in the data.
n_users = df.UserID.unique().shape[0] 

#create a variable n_items to find the total number of unique movies in the data
n_items = df['ItemId '].unique().shape[0] 

#print the counts of unique users and movies
print(str(n_users) + ' users') 

print(str(n_items) + ' movies') 

#create a zero value matrix of size (n_users X n_items) to store the ratings in the cell of the matrix ratings.
ratings = np.zeros((n_users, n_items)) 

# for each tuple in the dataframe, df extract the information of each column of the row and store into the rating matrix cell value as below
for row in df.itertuples():
	ratings[row[1]-1, row[2]-1] = row[3] 

type(ratings)
ratings.shape
ratings

sparsity = float(len(ratings.nonzero()[0]))
sparsity /= (ratings.shape[0] * ratings.shape[1])
print 'Sparsity: {:4.2f}%'.format(sparsity*100)


from sklearn.cross_validation import train_test_split 
ratings_train, ratings_test = train_test_split(ratings,test_size=0.33, random_state=42)
ratings_test.shape

import numpy as np
import sklearn

dist_out = 1-sklearn.metrics.pairwise.cosine_distances(ratings_train)
type(dist_out)
dist_out.shape

dist_out
user_pred = dist_out.dot(ratings_train) / np.array([np.abs(dist_out).sum(axis=1)]).T

from sklearn.metrics import mean_squared_error
def get_mse(pred, actual):
    # Ignore nonzero terms.
    pred = pred[actual.nonzero()].flatten()
    actual = actual[actual.nonzero()].flatten()
    return mean_squared_error(pred, actual)

get_mse(user_pred, ratings_train)

get_mse(user_pred, ratings_test)
#Find top N nearest neighbours

k=5
from sklearn.neighbors import NearestNeighbors

#define  NearestNeighbors object by passing k and the similarity method as parameters.
neigh = NearestNeighbors(k,'cosine')

#fit the training data to the nearestNeighbor object
neigh.fit(ratings_train)

#calculate the top5 similar users for each user and their similarity  values, i.e. the distance values between each pair of users.
top_k_distances,top_k_users = neigh.kneighbors(ratings_train, return_distance=True)

top_k_distances.shape
top_k_users.shape
top_k_users[0]
#create a zero matrix of same shape of the ratings_training
user_pred_k = np.zeros(ratings_train.shape)
#for each row in the ratings traing
for i in range(ratings_train.shape[0]):
	#make the user_pred row = dot product (top 5 closest users distances * predictions of all movies) / (each divided by sum of distances)
    user_pred_k[i,:] =   top_k_distances[i].T.dot(ratings_train[top_k_users][i])/np.array([np.abs(top_k_distances[i].T).sum(axis=0)]).T


user_pred_k.shape
user_pred_k

get_mse(user_pred_k, ratings_train)

get_mse(user_pred_k, ratings_test)

#Since we have to calculate the similarity between movies, we use movie count as k instead of user count
k = ratings_train.shape[1]
neigh = NearestNeighbors(k,'cosine')
#we fit the transpose of the rating matrix to the Nearest Neighbors object
neigh.fit(ratings_train.T)
#calcualte the cosine similarity distance between each movie pairs
top_k_distances,top_k_users = neigh.kneighbors(ratings_train.T, return_distance=True)
top_k_distances.shape

item__pred = ratings_train.dot(top_k_distances) / np.array([np.abs(top_k_distances).sum(axis=1)])
item__pred.shape
item__pred

get_mse(item_pred, ratings_train)
get_mse(item_pred,ratings_test)

k = 40
neigh2 = NearestNeighbors(k,'cosine')
neigh2.fit(ratings_train.T)
top_k_distances,top_k_movies = neigh2.kneighbors(ratings_train.T, return_distance=True)

#rating prediction - top k user based

pred = np.zeros(ratings_train.T.shape)
for i in range(ratings_train.T.shape[0]):
    pred[i,:] = top_k_distances[i].dot(ratings_train.T[top_k_users][i])/np.array([np.abs(top_k_distances[i]).sum(axis=0)]).T

	
get_mse(item_pred_k, ratings_train)
get_mse(item_pred_k,ratings_test)
























