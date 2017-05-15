//Starter script -- not to use in production -- needs more research

//load file from "C:\Users\christopher.brossman\Documents\Neo4j\default.graphdb\import"
LOAD CSV WITH HEADERS FROM "file:///C:/udata.csv" AS line  
//merge will create User node from UserId column from original data
MERGE (U:USER {USERID : line.UserId})
//with command will take User node and line object to the next part of the query
WITH line, U
//Movie node using merge and line id object
MERGE (M:MOVIE {ITEMID : line.MovieId})
//carry forward movie and user nodes and line object
WITH line,M,U
//create relation between USER and MOVIE node 
MERGE (U)-[:hasRated{RATING:line.Rating}]->(M);
//Added 2625 labels, created 2625 nodes, set 102625 properties, created 100000 relationships, statement executed in 200292 ms.

//returns a graph of ratings
MATCH (U:USER)-[R:hasRated]->(M:MOVIE) RETURN R



//Recommendation using simple co-rated movies by similar users:
//steps: 
// 1. extract pair of users who have rate same movies
// 2. take count of commonly rated movies by each pair of users
// 3. higher the commonly rated movie count, the more similar two users are to each other
// 4. extract all movies which similar users have rated, but which have not been rated by the active user,
//		and usggest these new movies as recomendations to the active user

//for each user (say user 1) who has rated a movie (say movie one) select all the users (say user 2) who have also rated movie1. 
// for user2 , also extract other movies rated by him, apart from movie 1
match(u1:USER)-[:hasRated]->(i1:MOVIE)<-[:hasRated]-(u2:USER)-[:hasRated]->(i2:MOVIE)

//carry similar users (u1,u2), calculating the count of co-rated movies by u1,u2 and extracting shared movies by u1,u2
with u1,u2, count(i1) as cnt , collect(i1) as Shareditems,i2

// filter where choose movies that not rated by u1 and count of co-rated movies > 2
where not(u1-[:hasRated]->i2) and u1.USERID='186' and cnt > 2 

//return recommendations
return distinct i2.ITEMID as Recommendations