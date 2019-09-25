An anime recommender made using R. One method is through a Content-based Filtering approach, which in this case recommends the genres with the best ratings. It takes a user's preferences and tries to recommend a similar anime based on what genres the user likes. The other method implements a User-Based Collaborite Filtering approach. This bases the user's preferences and then recommends something that is similar to what they likes (i.e. person A likes x and person B also likes x, therefore person A might recommend something else they like to person B. It more closely represents a more-realistic approach. However, each method has its pros and cons. The scripts use recommenderlab to make the predictions and the working directory must be set to be able to access the ratings or list of anime being used. 

Acknowledgements: The list was taken from all rating in the MyAnimeList data. Collected by Kaggle user CooperUnion. 
