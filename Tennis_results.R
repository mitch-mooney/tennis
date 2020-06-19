library(dplyr)
library(plotly)
library(ggplot2)
library(ggpmisc)
library(MASS)
#source of files http://www.tennis-data.co.uk/ausopen.php
wta<-read.csv("Data/WTA_ausopen.csv", stringsAsFactors = F)
atp<-read.csv("Data/ATP_ausopen.csv", stringsAsFactors = F)

# General data cleaning
wta <- wta %>%
  rename(
    tour = WTA)
wta$tour <- "WTA"
wta <- wta %>% 
  mutate(total_sets = Wsets+Lsets) %>% 
  mutate(win_games = rowSums(cbind(W1, W2, W3), na.rm = T)) %>% 
  mutate(loss_games = rowSums(cbind(L1, L2, L3), na.rm = T)) %>% 
  mutate(prop_win_games = win_games/(win_games+loss_games)) %>% 
  mutate(prop_loss_games = loss_games/(win_games+loss_games)) %>% 
  mutate(Wmargin = (Wsets/total_sets)+prop_win_games) %>% 
  mutate(Lmargin = (Lsets/total_sets)+prop_loss_games)

atp <- atp %>%
  rename(
    tour = ATP)
atp$tour <- "ATP"

atp <- atp %>% 
  mutate(total_sets = Wsets+Lsets) %>% 
  mutate(win_games = rowSums(cbind(W1, W2, W3, W4, W5), na.rm = T)) %>% 
  mutate(loss_games = rowSums(cbind(L1, L2, L3, L4, L5), na.rm = T)) %>% 
  mutate(prop_win_games = win_games/(win_games+loss_games)) %>% 
  mutate(prop_loss_games = loss_games/(win_games+loss_games)) %>% 
  mutate(Wmargin = (Wsets/total_sets)+prop_win_games) %>% 
  mutate(Lmargin = (Lsets/total_sets)+prop_loss_games)
  
wta <- wta %>% 
  select(tour, Tournament, Date, Round, Winner, Loser, WRank, LRank, WPts, LPts, Comment, B365W, B365L,total_sets,Wsets, Lsets, win_games, loss_games, prop_win_games, prop_loss_games, Wmargin,Lmargin) 
atp <- atp %>% 
  select(tour, Tournament, Date, Round, Winner, Loser, WRank, LRank, WPts, LPts, Comment, B365W, B365L,total_sets,Wsets, Lsets, win_games, loss_games, prop_win_games, prop_loss_games, Wmargin,Lmargin) 

aus_open<-rbind(atp, wta)
aus_open$WRank <- as.integer(aus_open$WRank)
aus_open$LRank <- as.integer(aus_open$LRank)
aus_open$WPts <- as.integer(aus_open$WPts)
aus_open$LPts <- as.integer(aus_open$LPts)

aus_open$Date <- as.Date(aus_open$Date, "%d/%m/%y")

#create numerical metrics for favorite winning and higher ranked player winning
aus_open <- aus_open %>% 
  mutate(favorite = ifelse(B365W > B365L, 0, 1))%>%
  mutate(rank_win = ifelse(WRank > LRank, 0, 1))
aus_open$favorite <- as.factor(aus_open$favorit)
aus_open$rank_win <- as.factor(aus_open$rank_win)
aus_open$year = as.character(format(aus_open$Date, format = "%Y"))

aus_open$Tour <- as.numeric(ordered(aus_open$tour, levels = c("ATP", "WTA")))
aus_open$round <- as.numeric(ordered(aus_open$Round, levels = c("1st Round","2nd Round","3rd Round","4th Round","Quarterfinals","Semifinals","The Final")))
#randomly assign 10% of people as ill
aus_open$W_ill <- as.character(rbinom(n=2794, size=1, prob=0.05))
aus_open$L_ill <- as.character(rbinom(n=2794, size=1, prob=0.05))

# Generate variables to include into a neural network
aus_open$rand <- sample(100, size = nrow(aus_open), replace = TRUE)
aus_open <- aus_open %>% 
  mutate(player_A = ifelse(rand > 49.5, Winner, Loser)) %>% 
  mutate(player_B = ifelse(player_A == Winner, Loser, Winner)) %>% 
  mutate(player_A_rank = ifelse(player_A == Winner, WRank, LRank)) %>% 
  mutate(player_B_rank = ifelse(player_B == Winner, WRank, LRank)) %>% 
  mutate(player_A_odds = ifelse(player_A == Winner, B365W, B365L)) %>% 
  mutate(player_B_Odds = ifelse(player_B == Winner, B365W, B365L)) %>%
  mutate(player_A_sets = ifelse(player_A == Winner, Wsets, Lsets)) %>% 
  mutate(player_B_sets = ifelse(player_B == Winner, Wsets, Lsets)) %>%
  mutate(player_A_games = ifelse(player_A == Winner, win_games, loss_games)) %>% 
  mutate(player_B_games = ifelse(player_B == Winner, win_games, loss_games)) %>% 
  mutate(player_A_gamesProp = ifelse(player_A == Winner, prop_win_games, prop_loss_games)) %>% 
  mutate(player_B_gamesProp = ifelse(player_B == Winner, prop_win_games, prop_loss_games)) %>% 
  mutate(player_A_margin = ifelse(player_A == Winner, Wmargin, Lmargin)) %>% 
  mutate(player_B_margin = ifelse(player_B == Winner, Wmargin, Lmargin)) %>% 
  mutate(result = ifelse(player_A == Winner, 0, 1)) # player A wins = 0, player B wins = 1
#select only the variables of interest 
tennis<-aus_open %>% 
  select(result, player_A, player_B, Tour,year, round, Comment, player_A_rank, player_B_rank, player_A_odds, player_B_Odds,
         player_A_sets, player_B_sets, player_A_games,player_B_games,  player_A_gamesProp, player_B_gamesProp, player_A_margin, 
         player_B_margin)
tennis<-tennis[complete.cases(tennis), ] #remove NAs from data frame
#select only target and input variables for the model
model<-tennis %>% 
  select(result, Tour,round,player_A_rank,player_B_rank,player_A_odds,player_B_Odds)

library(keras) # deep learning library

data <- model
col_num<-as.numeric(ncol(data)) #number of columns just to make it easier if you change variable numbers
data[1:col_num] <- lapply(data[1:col_num], as.numeric) #make sure all variables are numeric
# Check the data matrix by plotting
table(data$result)
barplot(prop.table(table(data$result)),
        col = rainbow(3),
        ylim = c(0, 1),
        ylab = 'proportion',
        xlab = 'score events',
        cex.names = 1.5)

# make the data a matrix and normalize the data
data <- as.matrix(data)
dimnames(data) <- NULL # remove column names
data[,2:col_num] <- normalize(data[,2:col_num])

# organize training set and testing set
set.seed(321)
ind<-sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
training <- data[ind==1, 2:col_num]
test <- data[ind==2 , 2:col_num]
trainingtarget <- data[ind==1, 1]
testtarget <- data[ind==2, 1]
trainLabels <- to_categorical(trainingtarget)
testLabels <- to_categorical(testtarget)
test_var<-as.numeric(nrow(testLabels))
test_dim<-as.numeric(ncol(testLabels))

### --- Winning Model --- ### 74.4% accuracy on test data on 1500 EPOCHS
#configure model
model <- keras_model_sequential()
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(col_num-1)) %>% 
  layer_dropout(rate = 0.8) %>% 
  layer_dense(units = 128, activation = 'sigmoid') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'sigmoid') %>% 
  layer_dropout(rate = 0.2) %>% 
  layer_dense(units = 32, activation = 'relu') %>% 
  layer_dropout(rate = 0.1) %>% 
  layer_dense(units = 2, activation = 'softmax')
summary(model)
#compile model choose an optimizer
model %>% 
  compile(loss = 'binary_crossentropy',
          optimizer = optimizer_adam(lr=0.002),
          #optimizer = optimizer_sgd(lr = 0.002),
          #optimizer = optimizer_rmsprop(lr = 0.003),
          metrics = 'accuracy')

history <- model %>% 
  fit(training,
      trainLabels,
      epochs = 1000,
      batch_size = 128,
      validation_split = 0.3)

plot(history)

#evalute model from test dataset
model %>% 
  evaluate(test, testLabels)
# look at the model prediction probabilities
prob<- model %>% 
  predict_proba(test)
#predict test data targets
pred <- model %>% 
  predict_classes(test)
#create a confusion matrix using absolute values
table(Predicted = pred, Actual = testtarget)


#put down predictions for all matches to add to score_margin dataframe
x<-data[,2:col_num]
all_match_pred<-model %>% predict_proba(x)

tennis<-cbind(tennis, all_match_pred)
names(tennis)[20] <- "PlayerA_win_Prob"
names(tennis)[21] <- "PlayerB_win_prob"
margin_data <- tennis %>%
  select(player_A_rank,PlayerA_win_Prob)

#plot correlation between win probability and Margin
my.formula <- y ~ x
margin_data%>%
  #filter(PlayerA_win_Prob >0.5)%>%
  ggplot(aes(x = PlayerA_win_Prob, y = player_A_margin))+
  geom_point()+
  geom_smooth(method = "glm", se=TRUE, color="blue", formula = my.formula) +
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE)   
# put prediction probabilities into categories
tennis<-tennis %>%
  mutate(pred_cat = ifelse(PlayerA_win_Prob < 0.1, 1, 
                           ifelse(PlayerA_win_Prob > 0.1 & PlayerA_win_Prob < 0.2, 2,
                                  ifelse(PlayerA_win_Prob > 0.2 &PlayerA_win_Prob < 0.3, 3,
                                         ifelse(PlayerA_win_Prob > 0.3 & PlayerA_win_Prob < 0.4, 4,
                                                ifelse(PlayerA_win_Prob > 0.4 & PlayerA_win_Prob < 0.5, 5,
                                                       ifelse(PlayerA_win_Prob > 0.5 & PlayerA_win_Prob < 0.6, 6,
                                                              ifelse(PlayerA_win_Prob > 0.6 & PlayerA_win_Prob< 0.7, 7,
                                                                     ifelse(PlayerA_win_Prob > 0.7 & PlayerA_win_Prob < 0.8, 8,
                                                                            ifelse(PlayerA_win_Prob > 0.8 & PlayerA_win_Prob < 0.9, 9, 10))))))))))
# ensure categories are a factor
tennis$pred_cat <- as.factor(tennis$pred_cat)
#filter out all Retired matches
tennis <- tennis %>% 
  filter(Comment != "Retired")

# Find the mean, median and SD of proportion to matches won by prediction category; prediction category and mens(1)/womens(2) tour
library(plyr) # use plyr package
Toursum_pred_cat <- ddply(tennis, .(pred_cat, Tour), summarise, rating.mean=mean(player_A_gamesProp), rating.sd = sd(player_A_gamesProp), rating.median = median(player_A_gamesProp))
Sum_pred_cat <- ddply(tennis, .(pred_cat), summarise, rating.mean=mean(player_A_gamesProp), rating.sd = sd(player_A_gamesProp), rating.median = median(player_A_gamesProp))

detach("package:plyr", unload = TRUE) #plyr package can interrupt dplyr so just dettach it just in case

# plot distribution of proportion of matches won by prediction category
ggplot(tennis, aes(x=player_A_gamesProp, color=pred_cat)) +
  geom_histogram(aes(y = ..density..),fill="white", position="dodge")+
  geom_density(alpha = 0.3)+
  geom_vline(data=Sum_pred_cat, aes(xintercept=rating.mean,  colour=pred_cat),
             linetype="dashed", size=1)+
  theme(legend.position="top")+
  xlim(0,1)+
  facet_grid(pred_cat ~.)

#write.csv(tennis, 'tennis.csv')

# plot distribution of proportion of matches won by prediction category and tour
ggplot(tennis, aes(x=player_A_gamesProp, color=pred_cat)) +
  geom_histogram(aes(y = ..density..),fill="white", position="dodge")+
  geom_density(alpha = 0.3)+
  geom_vline(data=Toursum_pred_cat, aes(xintercept=rating.mean,  colour=pred_cat),
             linetype="dashed", size=1)+
  theme(legend.position="top")+
  xlim(0,1)+
  facet_grid(pred_cat ~Tour)

  