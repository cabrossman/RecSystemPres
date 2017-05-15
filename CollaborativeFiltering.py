####################################################################################
# Warning - parts of this application are not finished. Some metrics need reworking
####################################################################################


#Collaborative Filtering using Python
#http://files.grouplens.org/datasets/movielens/ml-100k.zip
import os
import numpy as np
import pandas as pd
import sklearn
os.chdir('C:\\Users\\christopher.brossman\\Documents\\Projects\\work\\Recommender System\\Rec System Built\\data')
trans_small = pd.read_csv('CK_LoyaltyTrans_2016Ohio.csv')
upc_attr = pd.read_csv('UPC_attributes.csv')

df = pd.merge(trans_small, upc_attr, how='inner', on=None, left_on='TransactionItems_ItemBarcode', right_on='Product UPC')  
 
#create a variable n_users to find the total number of unique users in the data.
userData = pd.DataFrame(df.ConsumerID.unique(),columns = ['ConsumerID'])
userData['unique_user_id'] = userData.index
n_users = userData.shape[0] 

#create a variable n_items to find the total number of unique products in the data
productData = pd.DataFrame(df.Product.unique(),columns = ['Product'])
productData['unique_product_id'] = productData.index
n_items = productData.shape[0] 

#bring in unique_user_id
df = pd.merge(df, userData, how='inner', on='ConsumerID')
#bring in unique_product_id
df = pd.merge(df, productData, how='inner', on='Product')


#print the counts of unique users and products
print(str(n_users) + ' users') 
print(str(n_items) + ' products') 

#create a zero value matrix of size (n_users X n_items) to store the ratings in the cell of the matrix ratings.
ratings = np.zeros((n_users, n_items)) 

# for each tuple in the dataframe, df extract the information of each column of the row and store into the rating matrix cell value as below
#this creates user-item matrix
for row in df.itertuples():
	ratings[row[len(df.columns) - 1], row[len(df.columns)]] = 1

type(ratings)
ratings.shape
ratings

#so very very sparse
sparsity = float(len(ratings.nonzero()[0]))
sparsity /= (ratings.shape[0] * ratings.shape[1])
print 'Sparsity: {:4.2f}%'.format(sparsity*100)

#split data set for testing
from sklearn.cross_validation import train_test_split 
ratings_train, ratings_test = train_test_split(ratings,test_size=0.33)
ratings_test.shape

#build the similarity matrix. I think it knows its binary data
dist_out = 1-sklearn.metrics.pairwise.cosine_distances(ratings_train)
type(dist_out)
dist_out.shape

dist_out

#take dot product between the distance and rating matrix then normalize
#this is the prediction!!!
# we weight the the ratings/purchases by the similarity, then divide by sum total to scale
user_pred = dist_out.dot(ratings_train) / np.array([np.abs(dist_out).sum(axis=1)]).T

from sklearn.metrics import mean_squared_error
def get_mse(pred, actual):
    # Ignore nonzero terms.
    pred = pred[actual.nonzero()].flatten()
    actual = actual[actual.nonzero()].flatten()
    return mean_squared_error(pred, actual)

	
#change these with error calculations for binary data. Confusion Matrix
	
#in sample MSE	-- should not use MSE, use a different metric. Update this
get_mse(user_pred, ratings_train)
#out of sample MSE -- should not use MSE, use a different metric. Update this
get_mse(user_pred, ratings_test)
#random MSE -- should not use MSE, use a different metric. Update this
get_mse(np.random.randint(2, size=ratings_train.shape), ratings_train)

#Find top N nearest neighbours to only use them for similarity prediction

k=5
from sklearn.neighbors import NearestNeighbors

#define  NearestNeighbors object by passing k and the similarity method as parameters.
neigh = NearestNeighbors(k,'Jaccard')

#fit the training data to the nearestNeighbor object
neigh.fit(ratings_train)

#calculate the top5 similar users for each user and their similarity  values, i.e. the distance values between each pair of users.

#will return two matricies of users by k closest users. The closest will be same user
top_k_distances,top_k_users = neigh.kneighbors(ratings_train, return_distance=True)

top_k_distances.shape
top_k_users.shape
top_k_users[0]
#create a zero matrix of same shape of the ratings_training
user_pred_k = np.zeros(ratings_train.shape)
#for each row in the ratings traing
#SUPER SLOW!!!!!!!!!!!!!!!
for i in range(ratings_train.shape[0]):
	#make the user_pred row = dot product (top 5 closest users distances * predictions of all movies) / (each divided by sum of distances)
	#top_k_distances[i].T is vector with 5 elements
	#ratings_train[top_k_users][i] = 5 row by P products (4333)
	#top_k_distances[i].T.dot(ratings_train[top_k_users][i]) = vector of length products (4333)
	#dot product returns a vectors of p elements which is sum per each product(simularity of 5 users*binary purch all products by 5 users)
	# to scale divide by length of vector
	print i
    user_pred_k[i,:] = top_k_distances[i].T.dot(ratings_train[top_k_users][i])/np.array([np.abs(top_k_distances[i].T).sum(axis=0)]).T
	

user_pred_k.shape #users by products. sparse matrix7723x4333
user_pred_k
#doesnt work -- need to update with categorical measure
get_mse(user_pred_k, ratings_train)
get_mse(user_pred_k, ratings_test)

#Since we have to calculate the similarity between products, we use item count as k instead of user count
k = ratings_train.shape[1]
neigh = NearestNeighbors(k,'jaccard')
#we fit the transpose of the rating matrix to the Nearest Neighbors object
neigh.fit(ratings_train.T)
#calcualte the cosine similarity distance between each product pairs
top_k_distances,top_k_users = neigh.kneighbors(ratings_train.T, return_distance=True)
top_k_distances.shape

item__pred = ratings_train.dot(top_k_distances) / np.array([np.abs(top_k_distances).sum(axis=1)])
item__pred.shape
item__pred

#need confusion matrix
get_mse(item_pred, ratings_train)
get_mse(item_pred,ratings_test)

#use closest similarity as 40 instead of 5 for users
k = 40
neigh2 = NearestNeighbors(k,'jaccard')
neigh2.fit(ratings_train.T)
top_k_distances,top_k_movies = neigh2.kneighbors(ratings_train.T, return_distance=True)

#rating prediction - top k user based

#very slow
pred = np.zeros(ratings_train.T.shape)
for i in range(ratings_train.T.shape[0]):
	print i
    pred[i,:] = top_k_distances[i].dot(ratings_train.T[top_k_users][i])/np.array([np.abs(top_k_distances[i]).sum(axis=0)]).T

#confusion matrix instead of mse
get_mse(item_pred_k, ratings_train)
get_mse(item_pred_k,ratings_test)
























