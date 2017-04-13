Titanic
================

``` r
library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('dplyr') # data manipulation
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library('mice') # imputation
library('randomForest') # classification algorithm
```

    ## randomForest 4.6-12

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

``` r
train  <- read.csv('train.csv', stringsAsFactors = F)
test <- read.csv('test.csv', stringsAsFactors = F)
full <- bind_rows(train,test)

# checek the data
str(full) 
```

    ## 'data.frame':    1309 obs. of  12 variables:
    ##  $ PassengerId: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Survived   : int  0 1 1 1 0 0 0 0 1 1 ...
    ##  $ Pclass     : int  3 1 3 1 3 3 1 3 3 2 ...
    ##  $ Name       : chr  "Braund, Mr. Owen Harris" "Cumings, Mrs. John Bradley (Florence Briggs Thayer)" "Heikkinen, Miss. Laina" "Futrelle, Mrs. Jacques Heath (Lily May Peel)" ...
    ##  $ Sex        : chr  "male" "female" "female" "female" ...
    ##  $ Age        : num  22 38 26 35 35 NA 54 2 27 14 ...
    ##  $ SibSp      : int  1 1 0 1 0 0 0 3 0 1 ...
    ##  $ Parch      : int  0 0 0 0 0 0 0 1 2 0 ...
    ##  $ Ticket     : chr  "A/5 21171" "PC 17599" "STON/O2. 3101282" "113803" ...
    ##  $ Fare       : num  7.25 71.28 7.92 53.1 8.05 ...
    ##  $ Cabin      : chr  "" "C85" "" "C123" ...
    ##  $ Embarked   : chr  "S" "C" "S" "S" ...

``` r
full$Title <- gsub('(.*, )|(\\..*)', '',full$Name   )
```

'(\\..*)select string after a DOT. DOT means any charcater * occuring 0 or more times. (.*, )select string precedes a Comma. So that selects "Cumings, .John Bradley" from the original string. Another gsub("^.*, (.*)\\..*$", "\\1", names) where (.\*) denotes the Term that you want to preserve in our exmaple its Mr'

``` r
# show Title count by sex
table(full$Title, full$Sex)
```

    ##               
    ##                female male
    ##   Capt              0    1
    ##   Col               0    4
    ##   Don               0    1
    ##   Dona              1    0
    ##   Dr                1    7
    ##   Jonkheer          0    1
    ##   Lady              1    0
    ##   Major             0    2
    ##   Master            0   61
    ##   Miss            260    0
    ##   Mlle              2    0
    ##   Mme               1    0
    ##   Mr                0  757
    ##   Mrs             197    0
    ##   Ms                2    0
    ##   Rev               0    8
    ##   Sir               0    1
    ##   the Countess      1    0

``` r
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don','Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title=='Mlle'] <- 'Miss'
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs'
full$Title[full$Title %in% rare_title ] <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)
```

    ##         
    ##          Master Miss  Mr Mrs Rare Title
    ##   female      0  264   0 198          4
    ##   male       61    0 757   0         25

``` r
# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name, 
                       function(x) strsplit(x, split ='[,]')[[1]][1])

nlevels(factor(full$Surname))
```

    ## [1] 875

``` r
#'It can be done as gsub("(,.*)","",full$Name).Split on COMMA. factor is used for categorical data. here nlevels is counting the total surnames'
```

``` r
# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp +full$Parch +1

full$Family <- paste(full$Surname, full$Fsize, sep = '-' )
head(full$Family) 
```

    ## [1] "Braund-2"    "Cumings-2"   "Heikkinen-1" "Futrelle-2"  "Allen-1"    
    ## [6] "Moran-1"

``` r
ggplot(full[1:891,], aes(x= Fsize, fill = factor(Survived)))+
  geom_bar(stat = 'count', position = 'dodge')+
  scale_x_continuous(breaks = c(1:20))+
  labs(x='Family size')+
  theme_few()
```

![](Titanic_files/figure-markdown_github/unnamed-chunk-8-1.png)

``` r
##891 are the total training data
```

``` r
full$FsizeD[full$Fsize==1] <-'singleton'
full$FsizeD[full$Fsize <5 & full$Fsize >1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

# Show family size by survival using a mosaic plot
mosaicplot(table(full$FsizeD, full$Survived), main= 'Family name by surname', shade = TRUE)
```

![](Titanic_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
#Let's enquire about the Passengers' CABIN
strsplit(full$Cabin[2], NULL)[[1]]
```

    ## [1] "C" "8" "5"

``` r
# Create a Deck variable. Get passenger deck A - F:
full$Deck <- factor(sapply(full$Cabin, function(x) strsplit(x, NULL)[[1]][1]))
```

<b> Sensible value imputation </b>

``` r
full$Embarked[full$Embarked == ""] <- NA

full[(which(is.na(full$Embarked))), 1]
```

    ## [1]  62 830

we get passanger 62, 830 is missing embarkment.

``` r
# Value of Embarked column in row 63 and 830 is NULL
full[c(62, 830),"Embarked"]
```

    ## [1] NA NA

``` r
full[c(62, 830), 'Fare'][[1]][1]
```

    ## [1] 80

``` r
full[c(62, 830), 'Fare'][[1]][1]
```

    ## [1] 80

Both passengers had first class tickets that they spent 80 (pounds?) on. Let’s see the embarkment ports of others who bought similar kinds of tickets.

``` r
full %>%
  group_by(Embarked, Pclass) %>%
  filter(Pclass == "1") %>%
  summarise(mfare = median(Fare),
            n = n())
```

    ## Source: local data frame [4 x 4]
    ## Groups: Embarked [?]
    ## 
    ##   Embarked Pclass   mfare     n
    ##      <chr>  <int>   <dbl> <int>
    ## 1        C      1 76.7292   141
    ## 2        Q      1 90.0000     3
    ## 3        S      1 52.0000   177
    ## 4     <NA>      1 80.0000     2

``` r
# Get rid of our missing passenger IDs
embark_fare <- full %>%
  filter(PassengerId != 62 & PassengerId != 830)
# Use ggplot2 to visualize embarkment, passenger class, & median fare
ggplot(embark_fare, aes(x= Embarked, y= Fare, fill=factor(Pclass)))+
  geom_boxplot()+
  geom_hline(aes(yintercept=80),
             color ='red',linetype='dashed', lwd=2) +
  scale_y_continuous(labels=dollar_format())
```

    ## Warning: Removed 1 rows containing non-finite values (stat_boxplot).

![](Titanic_files/figure-markdown_github/unnamed-chunk-16-1.png) Voilà! The median fare for a first class passenger departing from Charbourg (‘C’) coincides nicely with the $80 paid by our embarkment-deficient passengers. I think we can safely replace the NA values with ‘C’.

``` r
# Since their fare was $80 for 1st class, they most likely embarked from 'C'
full$Embarked[c(62,830)] <- 'C'
```

We’re close to fixing the handful of NA values here and there. Passenger on row 1044 has an NA Fare value.

``` r
ggplot(full[full$Pclass == '3' & full$Embarked == 'S', ], 
  aes(x = Fare)) +
  geom_density(fill = '#99d6ff', alpha=0.4) +
  geom_vline(aes(xintercept=median(Fare, na.rm=T)),
    colour='red', linetype='dashed', lwd=1)+
  scale_x_continuous(labels=dollar_format())
```

    ## Warning: Removed 1 rows containing non-finite values (stat_density).

![](Titanic_files/figure-markdown_github/unnamed-chunk-18-1.png) From this visualization, it seems quite reasonable to replace the NA Fare value with median for their class and embarkment which is $8.05.

``` r
# Replace missing fare value with median fare for class/embarkment
full$Fare[1044] <- median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)
```

<b>Predictive imputation </b>

There are quite a few missing Age values in our data.

``` r
sum(is.na(full$Age))
```

    ## [1] 263

We could definitely use rpart (recursive partitioning for regression) to predict missing ages, but I’m going to use the mice package for this task

``` r
# Make variables factors into factors
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'Title','Surname','Family','FsizeD')

full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

# Set a random seed
set.seed(129)

# Perform mice imputation, excluding certain less-than-useful variables:
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], method='rf') 
```

    ## 
    ##  iter imp variable
    ##   1   1  Age  Deck
    ##   1   2  Age  Deck
    ##   1   3  Age  Deck
    ##   1   4  Age  Deck
    ##   1   5  Age  Deck
    ##   2   1  Age  Deck
    ##   2   2  Age  Deck
    ##   2   3  Age  Deck
    ##   2   4  Age  Deck
    ##   2   5  Age  Deck
    ##   3   1  Age  Deck
    ##   3   2  Age  Deck
    ##   3   3  Age  Deck
    ##   3   4  Age  Deck
    ##   3   5  Age  Deck
    ##   4   1  Age  Deck
    ##   4   2  Age  Deck
    ##   4   3  Age  Deck
    ##   4   4  Age  Deck
    ##   4   5  Age  Deck
    ##   5   1  Age  Deck
    ##   5   2  Age  Deck
    ##   5   3  Age  Deck
    ##   5   4  Age  Deck
    ##   5   5  Age  Deck

``` r
mice_output <- complete(mice_mod)
```

Let’s compare the results we get with the original distribution of passenger ages to ensure that nothing has gone completely awry.

``` r
# Plot age distributions
par(mfrow=c(1,2))
hist(full$Age, freq = F, main='Age: Original Data', 
  col='darkgreen', ylim=c(0,0.04))
hist(mice_output$Age, freq=F, main='Age: MICE Output', 
  col='lightgreen', ylim=c(0,0.04))
```

![](Titanic_files/figure-markdown_github/unnamed-chunk-23-1.png) Things look good, so let’s replace our age vector in the original data with the output from the mice model.

``` r
# Replace Age variable from the mice model.
full$Age <- mice_output$Age

sum(is.na(full$Age))
```

    ## [1] 0

<b> Feature Engineering </b>

Now that we know everyone’s age, we can create a couple of new age-dependent variables: Child and Mother.A child will simply be someone under 18 years of age and a mother is a passenger who is 1) female, 2) is over 18, 3) has more than 0 children (no kidding!), and 4) does not have the title ‘Miss’.

``` r
# First we'll look at the relationship between age & survival
ggplot(full[1:891,], aes(Age, fill = factor(Survived)))+
  geom_histogram() +
  facet_grid(.~Sex)
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

![](Titanic_files/figure-markdown_github/unnamed-chunk-25-1.png)

``` r
# Create the column child, and indicate whether child or adult
full$Child[full$Age<18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

# Show counts
table(full$Child, full$Survived)
```

    ##        
    ##           0   1
    ##   Adult 484 274
    ##   Child  65  68

``` r
# Adding Mother variable
full$Mother <- 'Not Mother'
full$Mother[full$Sex=='female' & full$Parch >0 & full$Age > 18 & full$Title != 'Miss' ] <- 'Mother'

# Show counts
table(full$Mother, full$Survived)
```

    ##             
    ##                0   1
    ##   Mother      16  39
    ##   Not Mother 533 303

``` r
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)
```

All of the variables we care about should be taken care of and there should be no missing data. I’m going to double check just to be sure:

``` r
md.pattern(full)
```

    ## Warning in data.matrix(x): NAs introduced by coercion

    ## Warning in data.matrix(x): NAs introduced by coercion

    ## Warning in data.matrix(x): NAs introduced by coercion

    ##     PassengerId Pclass Sex Age SibSp Parch Fare Embarked Title Surname
    ## 150           1      1   1   1     1     1    1        1     1       1
    ##  61           1      1   1   1     1     1    1        1     1       1
    ##  54           1      1   1   1     1     1    1        1     1       1
    ## 511           1      1   1   1     1     1    1        1     1       1
    ##  30           1      1   1   1     1     1    1        1     1       1
    ## 235           1      1   1   1     1     1    1        1     1       1
    ## 176           1      1   1   1     1     1    1        1     1       1
    ##  92           1      1   1   1     1     1    1        1     1       1
    ##               0      0   0   0     0     0    0        0     0       0
    ##     Fsize Family FsizeD Child Mother Ticket Survived Deck Name Cabin     
    ## 150     1      1      1     1      1      1        1    1    0     0    2
    ##  61     1      1      1     1      1      1        0    1    0     0    3
    ##  54     1      1      1     1      1      0        1    1    0     0    3
    ## 511     1      1      1     1      1      1        1    0    0     0    3
    ##  30     1      1      1     1      1      0        0    1    0     0    4
    ## 235     1      1      1     1      1      1        0    0    0     0    4
    ## 176     1      1      1     1      1      0        1    0    0     0    4
    ##  92     1      1      1     1      1      0        0    0    0     0    5
    ##         0      0      0     0      0    352      418 1014 1309  1309 4402

We have finally finished treating all of the relevant missing values in the Titanic dataset which has included some fancy imputation with mice.

``` r
# Split the data back into a train set and a test set
train <- full[1:891,]
test <- full[892:1309,]
```

<b> Building the model </b>

``` r
# Set a random seed
set.seed(754)
# Build the model (note: not all possible variables are used)
rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + 
                                            Fare + Embarked + Title + 
                                            FsizeD + Child + Mother,
                                            data = train)

# Show model error
plot(rf_model, ylim=c(0,0.36))
legend('topright', colnames(rf_model$err.rate), col=1:3, fill=1:3)
```

![](Titanic_files/figure-markdown_github/unnamed-chunk-31-1.png) The black line shows the overall error rate which falls below 20%. The red and green lines show the error rate for ‘died’ and ‘survived’ respectively. We can see that right now we’re much more successful predicting death than we are survival.

<b> Variable importance </b> Relative variable importance by plotting the Mean decrease in "Gini" calculated across all trees.

``` r
# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
varImportance
```

    ##          Variables Importance
    ## Pclass      Pclass      30.05
    ## Sex            Sex      59.34
    ## Age            Age      47.77
    ## SibSp        SibSp      12.53
    ## Parch        Parch       8.03
    ## Fare          Fare      59.07
    ## Embarked  Embarked       9.73
    ## Title        Title      65.90
    ## FsizeD      FsizeD      17.34
    ## Child        Child       4.44
    ## Mother      Mother       2.23

``` r
# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank=paste0('#', dense_rank(desc(Importance))))
rankImportance
```

    ##    Variables Importance Rank
    ## 1     Pclass      30.05   #5
    ## 2        Sex      59.34   #2
    ## 3        Age      47.77   #4
    ## 4      SibSp      12.53   #7
    ## 5      Parch       8.03   #9
    ## 6       Fare      59.07   #3
    ## 7   Embarked       9.73   #8
    ## 8      Title      65.90   #1
    ## 9     FsizeD      17.34   #6
    ## 10     Child       4.44  #10
    ## 11    Mother       2.23  #11

``` r
# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x= reorder(Variables, Importance),
                           y=Importance, fill= Importance))+
  geom_bar(stat='identity')
```

![](Titanic_files/figure-markdown_github/unnamed-chunk-34-1.png) Our title variable! It has the highest relative importance out of all of our predictor variables.

``` r
# Predict using the test set
prediction <- predict(rf_model, test)
# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID= test$PassengerId, Survived= prediction)
head(solution)
```

    ##     PassengerID Survived
    ## 892         892        0
    ## 893         893        0
    ## 894         894        0
    ## 895         895        0
    ## 896         896        1
    ## 897         897        0
