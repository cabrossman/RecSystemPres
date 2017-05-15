require('tidyverse')
library(recommenderlab)
library(graphAM)

#####################################################################################################################################################################
# read in data and do some light transformations
#####################################################################################################################################################################

#read in data
trans_small <- read_csv(paste0(getwd(),'/data/CK_LoyaltyTrans_2016Ohio.csv'))
upc_attr <- read_csv(paste0(getwd(),'/data/UPC_attributes.csv'))

#creating a new product id -- just depends on what one wants to recommend. Here is another alternative
product_cat<- upc_attr %>% mutate(prod_val = paste0(`NACS CATEGORY`,'-',`NACS SUBCATEGORY`,'-',BRAND)) %>% distinct(prod_val) %>% 
  mutate(prod_id = 1:NROW(prod_val))

#combined
combined_dat <- inner_join(trans_small, upc_attr, by=c('TransactionItems_ItemBarcode'='Product UPC')) %>% 
  mutate(prod_val = paste0(`NACS CATEGORY`,'-',`NACS SUBCATEGORY`,'-',BRAND)) %>% inner_join(product_cat,by='prod_val')

#####################################################################################################################################################################
# options for user item matrix
#####################################################################################################################################################################

#user item matrix using created product id after JOINED with UPC info
# user_item_mat <- combined_dat %>% select(consumer = ConsumerID, item = prod_val) %>% mutate(yesno = 1) %>% distinct %>% spread(item,yesno, fill=0) %>% as.matrix()
# rownames(user_item_mat) <- user_item_mat[,1]
# user_item_mat <- user_item_mat[,2:NCOL(user_item_mat)]
# dim(user_item_mat) 
#11528X1558

#user item matrix using product after JOINED with UPC info. Making this a binary matrix
user_item_mat <- combined_dat %>% mutate(purch = 1) %>% select(consumer = ConsumerID, item = Product, purch) %>% 
  distinct() %>% spread(key = item, value = purch, fill=0) %>% as.matrix()
rownames(user_item_mat) <- user_item_mat[,1]
user_item_mat <- user_item_mat[,2:NCOL(user_item_mat)]
dim(user_item_mat) 
#11528X4333

## coerce it into a binaryRatingMatrix, then remove user_item_matrix for memory
#382.2 MB vs 1.7MB, much more efficient storage
b <- as(user_item_mat, "binaryRatingMatrix"); rm(user_item_mat)

#user item matrix using bar code no joining with UPC
# user_item_mat <- trans_small %>% select(consumer = ConsumerID, item = TransactionItems_ItemBarcode) %>% 
#   mutate(yesno = 1) %>% distinct %>% spread(item,yesno, fill=0) %>% as.matrix()
# rownames(user_item_mat) <- user_item_mat[,1]
# user_item_mat <- user_item_mat[,2:NCOL(user_item_mat)]
# dim(user_item_mat) 
#11528X5870

#here is an alternative using the count as the implicit rating rather than binary
user_item_mat_q <- combined_dat %>% select(consumer = ConsumerID, item = Product, q = TransactionItems_ItemQuantity) %>% 
  group_by(consumer, item) %>% summarise(q = sum(q)) %>% ungroup() %>% spread(item,q, fill=0) %>% as.matrix()
rownames(user_item_mat_q) <- user_item_mat_q[,1]
user_item_mat_q <- user_item_mat_q[,2:NCOL(user_item_mat_q)]
dim(user_item_mat_q) 
#this doesnt store efficiently though
q <- as(user_item_mat_q, "realRatingMatrix"); rm(user_item_mat_q)

#####################################################################################################################################################################
# look at summary stats, binary
#####################################################################################################################################################################

#top 5% of customers and products mapped
image(b[rowCounts(b) > quantile(rowCounts(b),.95), colCounts(b) > quantile(colCounts(b),.95)])


## use some methods defined in ratingMatrix
class(b)
dim(b)
dimnames(b)
## counts
rowCounts(b)
colCounts(b)

## plot
image(b)

#methods available in recommenderlab
methods(class = class(b))

#get methods
recommenderRegistry$get_entries(dataType = "binaryRatingMatrix")
names(recommenderRegistry$get_entries(dataType = "binaryRatingMatrix"))

#data exploration
customerPurchase <- as.data.frame(table(rowCounts(b))) %>% select(NumPurchases = Var1, Freq) %>% mutate(pct = Freq/sum(Freq), cum = cumsum(Freq)/sum(Freq))
n_users <- rowCounts(b)
qplot(n_users) + stat_bin(binwidth=5) + ggtitle("distribution of the number of items purchased by customers")
qplot(n_users[n_users <= 50]) + stat_bin(binwidth=5) + ggtitle("distribution of the number of items purchased by customers, customers with <50 purchases")

itemPurchase <- as.data.frame(table(colCounts(b))) %>% select(NumPurchases = Var1, Freq) %>% mutate(pct = Freq/sum(Freq), cum = cumsum(Freq)/sum(Freq))
n_items <- colCounts(b)
qplot(n_items) + stat_bin(binwidth=5) + ggtitle("distribution of the products purchased")
qplot(n_items[n_items <= 50]) + stat_bin(binwidth=5) + ggtitle("distribution of the products purchased, items with <50 purchases")

bsamp <- b[rowCounts(b)>=5,colCounts(b)>=5]
#bsamp <- b[names(rowCounts(b)>=5),names(colCounts(b)>=5)]
image(bsamp)

#data exploration
customerPurchaseSamp <- as.data.frame(table(rowCounts(bsamp))) %>% select(NumPurchases = Var1, Freq) %>% mutate(pct = Freq/sum(Freq), cum = cumsum(Freq)/sum(Freq))
n_users <- rowCounts(bsamp)
qplot(n_users) + stat_bin(binwidth=5) + ggtitle("distribution of the number of items purchased by customers")
qplot(n_users[n_users <= 50]) + stat_bin(binwidth=5) + ggtitle("distribution of the number of items purchased by customers, customers with <50 purchases")

itemPurchaseSamp <- as.data.frame(table(colCounts(bsamp))) %>% select(NumPurchases = Var1, Freq) %>% mutate(pct = Freq/sum(Freq), cum = cumsum(Freq)/sum(Freq))
n_items <- colCounts(bsamp)
qplot(n_items) + stat_bin(binwidth=5) + ggtitle("distribution of the products purchased")
qplot(n_items[n_items <= 50]) + stat_bin(binwidth=5) + ggtitle("distribution of the products purchased, items with <50 purchases")

#####################################################################################################################################################################
#split to training and test based on 
#####################################################################################################################################################################
bin_matrix <- b

which_train <- sample(x = c(TRUE,FALSE), size = nrow(bin_matrix), replace = TRUE, prob = c(.8,.2))
recc_data_train <- bin_matrix[which_train,]
recc_data_test <- bin_matrix[!which_train,]


#####################################################################################################################################################################
# create popularity based recommendations
#####################################################################################################################################################################
model = "POPULAR"
params = NULL

pop_r <- Recommender(recc_data_train, method = model, parameter = params)
#The model can be obtained from a recommender using getModel().
names(getModel(pop_r))

#predict using predict method in R, makes 5 predictions
pop_recom <- predict(pop_r, recc_data_test, n=5)
#prediction list
pop_list <- as(pop_recom, "list")

pop_list$`203406416`
table(sort(unlist(lapply(pop_list,length)),decreasing = TRUE)) #-- how many recommendations per user?

#####################################################################################################################################################################
# create ALS based recommendations
#####################################################################################################################################################################
model = "ALS"
params = list(lambda = .1, alpha = 10, n_factors = 10, n_iterations = 10, min_item_nr = 1, seed = NULL)

ALS_r <- Recommender(recc_data_train, method = model, parameter = params)

#The model can be obtained from a recommender using getModel().
names(getModel(ALS_r))

#predict using predict method in R, makes 5 predictions
ALS_recom <- predict(ALS_r, recc_data_test, n=5)
#prediction list
ALS_list <- as(ALS_recom, "list")

ALS_list$`203406416`

#####################################################################################################################################################################
# create AR based recommendations - need to adjust because of rules
#####################################################################################################################################################################
#model = "AR"
#params = list(support = .1, confidence = .8, maxlen = 3, sort_measure = 'confidence', sort_decreasing = TRUE
              #, apriori_control = list(), verbose = FALSE
#              )

#AR_r <- Recommender(recc_data_train, method = model, parameter = params)
#The model can be obtained from a recommender using getModel().
#names(getModel(AR_r))

#predict using predict method in R, makes 5 predictions
#AR_recom <- predict(AR_r, recc_data_test, n=5)
#prediction list
#AR_list <- as(AR_recom, "list")

#####################################################################################################################################################################
# create IBCF based recommendations
#####################################################################################################################################################################
model = "IBCF"
params = list(k = 30, method="Jaccard", normalize_sim_matrix = FALSE, alpha = .5)

IBCF_r <- Recommender(recc_data_train, method = model, parameter = params)

#get similarity matrix
dist_ratings <- as(IBCF_r@model$sim, 'matrix')
product_names <- rownames(dist_ratings)
dist_category <- dist_ratings
lookup <- combined_dat %>% select(Product, cat = `NACS SUBCATEGORY`) %>% distinct() %>% arrange(cat)
cnt = 1
for(p in product_names){
  print(paste0(cnt,' of ',NROW(product_names)))
  p_cat <- lookup %>% filter(Product == p) %>% select(cat)
  poss <- lookup %>% filter(cat == p_cat$cat) %>% select(Product)
  for(j in product_names){
    dist_category[p,j] <- ifelse(j %in% poss$Product,1,0)
  }
  cnt = cnt + 1
}

#image(dist_category); image(dist_ratings)
#check for same product vals
identical(dim(dist_category), dim(dist_ratings))
identical(rownames(dist_category), rownames(dist_ratings))
identical(colnames(dist_category),colnames(dist_ratings))
w <- .25
dist_tot <- dist_category*w + dist_ratings*(1-w)

IBCF_r@model$sim <- as(dist_tot, 'dgCMatrix')

#The model can be obtained from a recommender using getModel().
names(getModel(IBCF_r))

#predict using predict method in R, makes 5 predictions
IBCF_recom <- predict(IBCF_r, recc_data_test, n=5)
#prediction list
IBCF_list <- as(IBCF_recom, "list")

IBCF_list$`203406416`

#####################################################################################################################################################################
# create UBCF based recommendations
#####################################################################################################################################################################
model = "UBCF"
params = list(nn = 25, method="Jaccard", weighted = TRUE, sample = FALSE)
UBCF_r <- Recommender(recc_data_train, method = model, parameter = params)
#The model can be obtained from a recommender using getModel().
names(getModel(UBCF_r))

#predict using predict method in R, makes 5 predictions
UBCF_recom <- predict(UBCF_r, recc_data_test, n=5)
#prediction list
UBCF_list <- as(UBCF_recom, "list")
UBCF_list$`203406416`



#####################################################################################################################################################################
# Odds/Ends
#####################################################################################################################################################################
#filter for only customers with 5 total purchases and items with 5 total purchases. Sample 90% of them
#bsamp <- sample(b[rowCounts(b) >=5,colCounts(b)>=5], NROW(b[rowCounts(b) >10,]) *.9)

#sampling for products and customers with greater than 3 interactions
bsamp <- b[rowCounts(b) >=3,colCounts(b)>=3]

#no sampling
#bsamp <- b



#image
# image(bsamp)

#how many users/items have 0 now that we have filtered
# sum(rowCounts(bsamp)==0); sum(colCounts(bsamp)==0)

#####################################################################################################################################################################
# Evaluation
#####################################################################################################################################################################
#create evaluation method
#given is the number of items to give to recommender for prediction, rest are held for computing error. So we have filtered the 
# group we know everyone has purchased atleast 3 products, so we are given 2 to the recommender and 1 is held out for computing error
e <- evaluationScheme(bsamp, method="split", train=0.8, k=2, given=2)
e <- evaluationScheme(bsamp, method="cross-validation", train=0.8, k=2, given=2)

## different models
ubcf_r <- Recommender(getData(e, "train"), "UBCF")
ibcf_r <- Recommender(getData(e, "train"), "IBCF")

## create predictions for the test data using known ratings (see given above)
ubcf_p <- predict(ubcf_r, getData(e, "known"), type="topNList", n=2)
ibcf_p <- predict(ibcf_r, getData(e, "known"), type="topNList", n=2)

ubcf_p_l <- as(ubcf_p, "list")
ibcf_p_l <- as(ibcf_p, "list")


error <- rbind(
  UBCF = calcPredictionAccuracy(ubcf_p, getData(e, "unknown"), given=2),
  IBCF = calcPredictionAccuracy(ibcf_p, getData(e, "unknown"), given=2)
  )

errorUserUBCF = calcPredictionAccuracy(ubcf_p, getData(e, "unknown"), given=2,byUser=TRUE)
errorUserIBCF = calcPredictionAccuracy(ibcf_p, getData(e, "unknown"), given=2,byUser=TRUE)

model_to_evaluate <- "UBCF"
results <- evaluate(x = e, method = model_to_evaluate, n = seq(10, 10000, 10))
head(getConfusionMatrix(results)[[1]])

columns_to_sum <- c("TP", "FP", "FN", "TN")
indices_summed <- Reduce("+", getConfusionMatrix(results))[, columns_to_sum]
head(indices_summed)
plot(results, annotate = TRUE, main = "ROC curve")


#####################################################################################################################################################################
# Evaluation - many models
# #####################################################################################################################################################################
# vector_k <- c(5, 25, 50, 75)
# alpha <- seq(.3, .5, .7)
# models <- list()
# 
# model1 <- lapply(vector_k, function(k,l){ list(name = "IBCF", param = list(method = "Jaccard", k = k, alpha = .3)) })
# names(model1) <- paste0("IBCF_k_", vector_k,'_alpha_',.3)  
# 
# model2 <- lapply(vector_k, function(k,l){ list(name = "IBCF", param = list(method = "Jaccard", k = k, alpha = .5)) })
# names(model2) <- paste0("IBCF_k_", vector_k,'_alpha_',.5)  
# 
# model3 <- lapply(vector_k, function(k,l){ list(name = "IBCF", param = list(method = "Jaccard", k = k, alpha = .7)) })
# names(model3) <- paste0("IBCF_k_", vector_k,'_alpha_',.7)  
# 
# model4 <- lapply(vector_k, function(k,l){   list(name = "UBCF", param = list(method = "Jaccard", nn = k))}) 
# names(model4) <- paste0("UBCF_k_", vector_k)  
# 
# lambda_list = c(.05,.1,.2)
# model5 <- lapply(lambda_list, function(lam,l){ list(name = "ALS", param = list(lambda = lam, alpha = 10, n_factors = 5, min_item_nr = 1, seed = NULL)) })
# names(model5) <- paste0("ALS_", lambda_list,'_factor_5')  
# 
# model6 <- lapply(lambda_list, function(lam,l){ list(name = "ALS", param = list(lambda = lam, alpha = 10, n_factors = 15, min_item_nr = 1, seed = NULL)) })
# names(model6) <- paste0("ALS_", lambda_list,'_factor_10')  
# 
# model7 <- list("random items" = list(name="RANDOM", param=NULL),"popular items" = list(name="POPULAR", param=NULL))
# 
# for(i in 1:7){
#   models <- append(models, get(paste0('model',i)))
# }


#just IBCF, UBCF and pop, rand models
vector_k <- c(5, 25, 50, 75)
alpha <- seq(.3, .5, .7)
models <- list()

model1 <- lapply(vector_k, function(k,l){ list(name = "IBCF", param = list(method = "Jaccard", k = k, alpha = .3)) })
names(model1) <- paste0("IBCF_k_", vector_k,'_alpha_',.3)  

model2 <- lapply(vector_k, function(k,l){ list(name = "IBCF", param = list(method = "Jaccard", k = k, alpha = .5)) })
names(model2) <- paste0("IBCF_k_", vector_k,'_alpha_',.5)  

model3 <- lapply(vector_k, function(k,l){ list(name = "IBCF", param = list(method = "Jaccard", k = k, alpha = .7)) })
names(model3) <- paste0("IBCF_k_", vector_k,'_alpha_',.7)  

model4 <- lapply(vector_k, function(k,l){   list(name = "UBCF", param = list(method = "Jaccard", nn = k))}) 
names(model4) <- paste0("UBCF_k_", vector_k)  

model5 <- list("random items" = list(name="RANDOM", param=NULL),"popular items" = list(name="POPULAR", param=NULL))

for(i in 1:5){
  models <- append(models, get(paste0('model',i)))
}

n_recs <- c(1, 3, 5, 10)
list_results <- evaluate(x = e, method = models, n = n_recs)
plot(list_results, annotate = 1:4, legend = 'topleft', lty=2); title('ROC')
#legend('topleft', legend = names(list_results),  adj = c(0, 0.6), cex=.2)
plot(list_results, 'prec/rec', annotate = 1, legend = 'bottomright'); title('prec-recall')



#####################################################################################################################################################################
# try to combine the distance matrix with the item descriptions via the following steps
# Create a recommender
# 1. define a similarity matrix based on purchases
# 2. Define a similarity matrix based on the item descriptions
# 3. Combine two matrixies
#####################################################################################################################################################################
