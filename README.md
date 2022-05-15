# Roulette-Analysis


https://htmlpreview.github.io/?https://github.com/KyrDas/Roulette-Analysis/blob/main/Roullete_Simulation.html



---
title: "Roulette analysis"
author: "Daskalopoulos Kyriakos"
output:
  html_document: default
---

```{r, echo = FALSE}
set.seed(123)
```

## Introduction

In this project we will calculate many important things about the game of roulette, and it will be proven in general that no-one can win an unfair game by collecting only probabilistic information and the past outcomes of the game.
Familiarity with stochastic mathematics and the game of roulette is assumed. 

## Bet Analysis

In this section, many roulette bets will be analyzed, and simulations will be done in order to understand the effect of each bet on the long-run. The parameters that we care about are the time of ruin(e.g. the time when the capital reaches zero or a value that cannot support the corresponding type of bet) and the exit capital(e.g. the amount of units the player has when he finishes playing a specific number of rounds).

$\S$ Use command set.seed(123) if you want to reproduce the results.

### Single number bet
We first turn our attention at one of the most basic bets available, that is, betting a chip at a single number. We know that the capital of the player at round n is

$$ X_n = \begin{cases}
X_{n-1} + 35 & p = 1/37\\
X_{n-1} -1, & q = 36/37\\
\end{cases}$$

The mean value of the profit,knowing all the information up to time $t =n-1$, is $\mathbb{E}(X_n|\mathcal{F_{n-1}}) = \frac{1}{37}(X_{n-1} + 35) + \frac{36}{37}(X_{n-1} - 1) = X_{n-1} - \frac{1}{37} \approx X_{n-1} - 0.027 \leq X_{n-1}$.

This is clearly a long-run losing bet, and a super-martingale. The variance is $Var(X_n|\mathcal{F}_{n-1}) = \mathbb{E}(X^{2}_n|\mathcal{F}_{n-1}) - \mathbb{E}^{2}(X_n|\mathcal{F}_{n-1}) = \frac{1}{37}(X_{n-1} + 35)^2 + \frac{36}{37}(X_{n-1} - 1)^2 - ( X_{n-1} - \frac{1}{37})^2 = 34,08035$

Hence, the standard deviation is $5.837838$, which is large, having in mind that we bet 1 unit.

```{r, echo = FALSE}
rounds = 250
initial_capital = 50
roullete_round<-sample(0:36, rounds, replace = T)
capital = matrix(0, 1, rounds + 1)
capital[1] = initial_capital
for(i in 1:rounds ){
  if(roullete_round[i] == 0){
    capital[i+1]=capital[i] + 35}
  else{
    capital[i+1]=capital[i] - 1
  }
}
x=seq(from=1, to=rounds + 1, by=1)
plot(x,capital,type="s", main="Graph of X_n ", xlab = "Rounds", ylab = "X_n", col = "red")
```

In the above graph, we see how the capital of the player varies per round. Here we assume that the capital can take negative values too, meaning that he can put more money in the game than the initial capital.

To understand how many players lose all their capital, we look at the following histogram.


```{r, echo = FALSE}
#Function that simulates betting a unit on zero(every round) with specific initial capital and rounds to play. 
#The function roullete_sim_zero returns a vector, of which the i-th element is the capital at the i-th round.
#The function roullete_sim_zero_ruin calculates the round on which the gambler goes bust. If he does not do so, it returns the number of rounds.
roullete_sim_number<-function(rounds, initial_capital){
roullete_round<-sample(0:36, rounds, replace = T)
capital = matrix(0, 1, rounds + 1)
capital[1] = initial_capital
for(i in 1:rounds){
  if(roullete_round[i] == 0){
    capital[i+1]=capital[i] + 35}
  else{
    capital[i+1]=capital[i] - 1
  }
}
#x=seq(from=1, to=rounds + 1, by=1)
#plot(x,capital,type="s", main="Roullete", xlab = "Rounds", ylab = "Capital", col = "red")
return(capital)
}
roullete_sim_number_ruin<-function(rounds, initial_capital){
  
  roullete_round<-sample(0:36, rounds, replace = T)
  
  capital = matrix(0, 1, rounds + 1)
  capital[1] = initial_capital
  ruin_round=rounds
  flag=0
  
  i=1
  while(i < rounds + 1){ 
    if(roullete_round[i] == 0){
      capital[i+1]=capital[i] + 35}
    else{
      capital[i+1]=capital[i] - 1
    }
    if(capital[i+1] < 0){
      ruin_round=i
      i=rounds
    }
    i=i+1
  }
  return(ruin_round)
}
roullete_sim_number_exitcap<-function(rounds, initial_capital){
  roullete_round<-sample(0:36, rounds, replace = T)
  
  lcapital = 0
  capital=matrix(0,1, rounds + 1)
  capital[1]= initial_capital
  i=1
  flag=0
  
  while(flag == 0){
   if(roullete_round[i] == 0){
    capital[i+1]=capital[i] + 35}
   else{
    capital[i+1]=capital[i] - 1
   }
   if(capital[i+1] < 0){
     capital[i+1] = 0
     flag=1
   }
   if(i == rounds){
     flag = 1
   }
  i=i+1
  }  
  
  lcapital=capital[rounds+1]
  return(lcapital)
}
num_sim=10000
initial_capital=50
rounds=250
ruin_time<-function(num_sim,initial_capital,rounds){
  ruin_time=matrix(0,num_sim,1)
  for (i in 1:num_sim){
   ruin_time[i] = roullete_sim_number_ruin(rounds, initial_capital)
  }
  return(ruin_time)
}
  
ruin = ruin_time(num_sim, initial_capital, rounds)
sum_ruin_time=0
ruin_amount=0
for (i in 1:num_sim){
  sum_ruin_time = sum_ruin_time + ruin[i]
  if (ruin[i] != 250){
    ruin_amount = ruin_amount + 1
  }
}
mean_ruin_time= sum_ruin_time/num_sim
exiting_capital<-function(num_sim,initial_capital, rounds){
  exiting_capital=matrix(0,num_sim, 1)
  for (i in 1:num_sim){
    exiting_capital[i] = roullete_sim_number_exitcap(rounds, initial_capital)
  }
  return(exiting_capital)
}
exit_cap = exiting_capital(num_sim, initial_capital, rounds)
winner_percent=0
for (i in 1:num_sim){
  if(exit_cap[i] > initial_capital){
    winner_percent = winner_percent + 1
  }
}
loser_percent = num_sim - winner_percent
```


```{r, echo = FALSE}
breaks1 = seq(from = 0, to = rounds, by = 2)
hist(ruin, breaks1, main = "Ruin rounds when betting at a single number", xlab = "Rounds")
```

According to the simulation we see that the mean ruin time is $`r mean_ruin_time`$(or the mean number of rounds played) and that $58.8\%$  of the players run out of capital before the end.

```{r, echo = TRUE}
breaks2 = seq(from = min(exit_cap), to = max(exit_cap) + 20, by = 10)
hist(exit_cap, breaks2, main = "Capital after all rounds", xlab = "Capital")
```

We note that, due to big variance, there are different "fates" for the players wishing to bet only on a single number per round. 
Out of 10000 simulations, $`r loser_percent`$ of them ended with less capital, but (as seen in the
histogram) there is a considerable amount of players which double, triple or even quintuple their initial
capital.


### Even payout bet

With the above term, I mean all the bets that if won, they give the player a profit that equals their
initial bet(e.g. Red/Black, Even/Odd).
The capital this time is

$$ X_n = \begin{cases}
+1 & 18/37\\
-1, & 19/37\\
\end{cases}$$

with mean value $\mathbb{E}(X_n|\mathcal{F_{n-1}}) = (X_{n-1} + 1)\frac{18}{37} + (X_{n-1} - 1)\frac{19}{37} = X_{n-1} - \frac{1}{37} \leq X_{n-1}$(same as before).

The variance is $Var(X_n|\mathcal{F}_{n-1}) = \frac{18}{37} (X_{n-1} + 1)^2+ \frac{19}{37}(X_{n-1} - 1)^2 - ( X_{n-1} - \frac{1}{37})^2 =  0.99926$, which is considerably smaller than that of the single number bet.

The following graph shows the capital of the player per round when betting in an even bet.

```{r,echo=FALSE}
rounds=250
initial_capital=50
roullete_round<-sample(0:36, rounds, replace = T)
  
x=c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
  
capital = matrix(0, 1, rounds + 1)
capital[1]= initial_capital
  
for(i in 1:rounds){
 if(roullete_round[i] %in% x == TRUE){
    capital[i+1]=capital[i] + 1}
 else{
    capital[i+1]=capital[i] - 1
    }
}
 
x=seq(from=1, to=rounds + 1, by=1)
plot(x,capital,type="s", main="Graph of X_n ", xlab = "Rounds", ylab = "X_n", col = "red")
```

Due to variance, it is a lot more stabilized than the single number bet.


```{r, echo = FALSE}
roullete_sim_red<-function(rounds, initial_capital){
  roullete_round<-sample(0:36, rounds, replace = T)
  
  x=c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
  
  capital = matrix(0, 1, rounds + 1)
  capital[1]= initial_capital
  
  for(i in 1:rounds){
    if(roullete_round[i] %in% x == TRUE){
      capital[i+1]=capital[i] + 1}
    else{
      capital[i+1]=capital[i] - 1
    }
  }
  return(capital)
}
roullete_sim_red_ruin<-function(rounds, initial_capital){
  
  roullete_round<-sample(0:36, rounds, replace = T)
  
  x=c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
  
  capital = matrix(0, 1, rounds + 1)
  capital[1]= initial_capital
  ruin_round=rounds
  flag=0
  
  i=1
  while(i < rounds + 1){
    if(roullete_round[i] %in% x == TRUE){
      capital[i+1]=capital[i] + 1}
    else{
      capital[i+1]=capital[i] - 1
    }
    if((capital[i] < 0) & (flag == 0) ){
      ruin_round=i
      i=rounds
    }
    i=i+1
  }
  return(ruin_round)
}
roullete_sim_red_exitcap<-function(rounds, initial_capital){
  roullete_round<-sample(0:36, rounds, replace = T)
  
  x=c(1,3,5,7,9,12,14,16,18,19,21,23,25,27,30,32,34,36)
  
  
  
  lcapital = 0
  capital=matrix(0,1, rounds + 1)
  capital[1]= initial_capital
  i=1
  flag=0
  
  while(flag == 0){
   if(roullete_round[i] %in% x == TRUE){
    capital[i+1]=capital[i] + 1}
   else{
    capital[i+1]=capital[i] - 1
   }
   if(capital[i+1] < 0){
     capital[i+1] = 0
     flag = 1
   }
   if(i == rounds){
     flag = 1
   }
  i=i+1
  }  
  
  lcapital=capital[rounds+1]
  return(lcapital)
}
num_sim=10000
initial_capital=50
rounds=250
ruin_time<-function(num_sim,initial_capital,rounds){
  ruin_time=matrix(0,num_sim,1)
  for (i in 1:num_sim){
   ruin_time[i] = roullete_sim_red_ruin(rounds, initial_capital)
  }
  return(ruin_time)
}
  
ruin = ruin_time(num_sim, initial_capital, rounds)
sum_ruin_time=0
ruin_amount=0
for (i in 1:num_sim){
  sum_ruin_time = sum_ruin_time + ruin[i]
  if (ruin[i] != rounds){
    ruin_amount = ruin_amount + 1
  }
}
mean_ruin_time= sum_ruin_time/num_sim
exiting_capital<-function(num_sim,initial_capital, rounds){
  exiting_capital=matrix(0,num_sim, 1)
  for (i in 1:num_sim){
    exiting_capital[i] = roullete_sim_red_exitcap(rounds, initial_capital)
  }
  return(exiting_capital)
}
exit_cap = exiting_capital(num_sim, initial_capital, rounds)
```


Turning our attention to the ruin time, we see that only the very “unlucky” players run out of capital before the end. In fact `r ruin_amount` players run out of capital before the end and the mean ruin time is `r mean_ruin_time`.

```{r, echo = FALSE}
breaks1 = seq(from = 0, to = rounds, by = 2)
hist(ruin, breaks1, main = "Ruin rounds when betting at even bets", xlab = "Rounds")
```

Finally, we see that the exiting capital is almost normal, with mean value equal to $50 - 250 \frac{1}{37} = 43.243$. Here we need to fit a distribution. Weibull or gamma.

```{r, echo=FALSE}
breaks2 = seq(from = 0, to = max(exit_cap) + 20, by = 10)
hist(exit_cap, breaks2, main = "Capital after all rounds", xlab = "Capital")
```



### Jeu Zero
The stochastic process that gives the profit on this kind of bet is
$$X_n = \begin{cases}
+14, & 4/37\\
+32, & 1/37\\
+8, & 3/37\\
-4, & 29/37\\
\end{cases}$$


The mean value of the profit,knowing all the information up to time $t =n-1$, is $\mathbb{E}(X_n|\mathcal{F_{n-1}})  \approx X_{n-1} - \frac{4}{37} \leq X_{n-1}$.

Here, the variance is $Var(X_n|\mathcal{F}_{n-1}) = \frac{4}{37}(X_{n-1} + 14)^2 + \frac{1}{37}(X_{n-1} + 32)^2 + \frac{3}{37}(X_{n-1} + 8)^2 + \frac{29}{37} (X_{n-1} - 4)^2 - (X_{n-1} - \frac{4}{37})^2 = \frac{2448}{37} \approx 66.162$, which is the largest that we have seen.

Like before, the following graphs give a general picture of this kind of bet. It is obvious since Jeu Zero has the less favorable long-term profit than the bets we examined before, that most players get ruined or have less capital when the game ends.


```{r, echo=FALSE}
roullete_sim_zeroplay<-function(rounds, initial_capital){
  roullete_round<-sample(0:36, rounds, replace = T)
  
  x=c(0,3,12,15,32,35)
  
  capital = matrix(0, 1, rounds + 1)
  capital[1]= initial_capital
  
  for(i in 1:rounds){
    if(roullete_round[i] %in% x == TRUE){
      capital[i+1]=capital[i] + 14}
    else if(roullete_round[i] == 26){
      capital[i+1]=capital[i] + 32
    }
    else{
      capital[i+1]=capital[i] - 4
    }
  }
  return(capital)
}
roullete_sim_zeroplay_ruin<-function(rounds, initial_capital){
  roullete_round<-sample(0:36, rounds, replace = T)
  
  x=c(0,3,12,15,32,35)
  
  capital = matrix(0, 1, rounds + 1)
  capital[1]= initial_capital
  ruin_round=rounds
  flag=0
  
  i=1
  while(i < rounds +1){
    if(roullete_round[i] %in% x == TRUE){
      capital[i+1]=capital[i] + 14}
    else if(roullete_round[i] == 26){
      capital[i+1]=capital[i] + 32
    }
    else{
      capital[i+1]=capital[i] - 4
    }
    if(capital[i+1] < 0){
      ruin_round=i
      i = rounds + 1
    }
    i = i +1
  }
  return(ruin_round)
} 
roullete_sim_zeroplay_exitcap<-function(rounds, initial_capital){
  roullete_round<-sample(0:36, rounds, replace = T)
  
  x=c(0,3,12,15,32,35)
  
  lcapital = 0
  capital=matrix(0,1, rounds + 1)
  capital[1]= initial_capital
  flag=0
  i=1
  
  while(flag == 0){
    if(roullete_round[i] %in% x == TRUE){
      capital[i+1]=capital[i] + 14}
    else if(roullete_round[i] == 26){
      capital[i+1]=capital[i] + 32
    }
    else{
      capital[i+1]=capital[i] - 4
    }
    if(capital[i+1] < 0){
     capital[i+1] = 0
     flag = 1
   }
   if(i == rounds){
     flag = 1
   }
  i=i+1
  } 
  lcapital=capital[rounds+1]
  return(lcapital)
}
num_sim=10000
initial_capital=20
rounds=100
ruin_time<-function(num_sim,initial_capital,rounds){
  ruin_time=matrix(0,num_sim,1)
  for (i in 1:num_sim){
   ruin_time[i] = roullete_sim_zeroplay_ruin(rounds, initial_capital)
  }
  return(ruin_time)
}
  
ruin = ruin_time(num_sim, initial_capital, rounds)
breaks1 = seq(from = 0, to = rounds, by = 2)
hist(ruin, breaks1,  main = "Ruin rounds when betting zeroplay", xlab = "Rounds")
exiting_capital<-function(num_sim,initial_capital, rounds){
  exiting_capital=matrix(0,num_sim, 1)
  for (i in 1:num_sim){
    exiting_capital[i] = roullete_sim_zeroplay_exitcap(rounds, initial_capital)
  }
  return(exiting_capital)
}
exit_cap = exiting_capital(num_sim, initial_capital, rounds)
breaks2 = seq(from = 0, to = max(exit_cap) + 20, by = 10)
hist(exit_cap, breaks2, main = "Capital after all rounds", xlab = "Capital")
```




## Miscellaneous

There are many more bets in a roullete game, like Voisin du Zero, Tiers du Cylindre, Dozen, triple number and so on. Based on the analysis done above, the reader can easily calculate the house edge for these bets.

Generally, one cannot find a bet with house edge less than $\frac{1}{37} = 0.027027027 \dots$.

The choice of the betting strategy depends on how risk averse a player is, but no one can hope to win in the long term, using statistical information. 
Many gamblers like to play  a strategy which I call "Exploiting the statisical limit" to minimize the losses, or even make profit. This is based on the following logical assumption: 

When a probabilistic phenomenon is happening, and a quantity of some nature is being observed(e.g. how much time someone loses in traffic jams each day), the empirical distribution must converge to the theoretical(we assume here a frequentist approach). 

Hence, when an anomaly in the empirical distribution is observed, for example the number zero is seen only 90,000 times in 370,000 spins of a non-crooked wheel, then we can expect that in the following 130,000 spins we will see zero more, because an histogram of 500,000 observations from a discrete uniform distribution looks like this:

```{r, echo FALSE}
k= sample(0:36, 500000, replace = TRUE)
breaks = seq(from = -1, to = 37, by = 1)
hist(k ,breaks)
```

Based on the above, suppose we have a player that observes 25,000 spins, notes the number that happens to arise less often and bets one chip on this number for the following 25,000 spins. The simulation below show that not only this is not a way to win, but it yields the same results with a "blind" even bet strategy.

```{r, echo = TRUE}
sim<-function(rounds, roullete, last_rounds){ 
  
  profit = 0
  
  
  d=roullete[1:last_rounds]
  d2=roullete[last_rounds:rounds]
  
  freq= matrix(0,1,37)
  for (i in 1:last_rounds){
    f=d[i] + 1
    freq[f] = freq[f] + 1
  }
  
  
  min_index = 1
  for (i in 1:37){
    k = freq[min_index] 
    if(freq[i] < k){
      min_index = i
    }
  }
  dif = rounds - last_rounds
  
  for (i in 1:dif){
    a = d2[i] + 1
    if (a == min_index){
      profit = profit + 35
    }
    else{ 
      profit = profit - 1}
  }
  return(profit)
}
rounds=50000
last_rounds = rounds/2
num_sim = 2000
prof_matr = matrix(0,1,num_sim)
kerdos_plein = matrix(0,1,num_sim)
for (i in 1:num_sim){
  roullete = sample(0:36, rounds, replace =  TRUE)
  prof_matr[i] = sim(rounds,roullete, last_rounds)
  for(j in last_rounds:rounds){
    if (roullete[j] == 0){
      kerdos_plein[i] = kerdos_plein[i] + 35
    }
    else{
      kerdos_plein[i] = kerdos_plein[i] -1
    }
  }
}
mprof = mean(prof_matr)
mplein = mean(kerdos_plein)
```

We see that the mean value of the profit(when playing 25,000 rounds) of the single number strategy is `r mplein` and that of the "Exploiting statistical limit" strategy is `r mprof`.

There are attempts to build a gambling strategy based on mechanical information of the wheel, which if made under cover, can make roullete a profitable game. These methods require knowledge of mechanics, so it will not be discussed here. The interested reader may refer to "Predicting the outcome of roulette :
Michael Small, Chi Kong Tse, https://arxiv.org/abs/1204.6412". 

## On making a game against a casino profitable


The most important question that arise when someone enters a casino is this: "Are there opportunities in
here for long-term profit?". 

This is a just question, but it can be instantly answered with a rhetorical question: "Does there exist a
private company that wants to give money to the people?".

For games in which the winning probabilities do not change(e.g. roullete, craps, blackjack with auto-shuffler) there exist the following theorem-like proposition.


$\S$ Let $\alpha_1, \dots, \alpha_n$ be a sequence of bounded random variables such that $\alpha_n$ is $\mathcal{F}_{n-1}$ measurable, and let $\xi_1, \dots, \xi_n, \dots$ be a supermartingale. Then the sequence $X_n = \sum_{k=1}^{n} \alpha_{k} (\xi_k - \xi_{k-1})$ is a supermartingale.

$\wp$ For the conditional mean value of $X_n$ we have $$ \mathbb{E}(X_n|\mathcal{F_{n-1}}) = \sum_{k=1}^{n} \mathbb{E}(  \alpha_{k} (\xi_k - \xi_{k-1}) | \mathcal{F}_{n-1})$$

By hypothesis, this is equal to $$ \sum_{k=1}^{n} \alpha_{k} \ \mathbb{E}(   (\xi_k - \xi_{k-1}) | \mathcal{F}_{n-1}) = \sum_{k=1}^{n} \alpha_{k} (\ \mathbb{E}(\xi_k | \mathcal{F}_{n-1}) - \xi_{k-1})$$

It is easy to see, since $\xi_i$ is a supermartingale that the above expression is $$ \leq \sum_{k=1}^{n-1} \alpha_{k} (\xi_k- \xi_{k-1}) = X_{n-1}$$. 

Hence, $X_n$ is a supermartingale. $\square$


Based on the above, we see that it is impossible to win in roullete using only past information. 

$\S$ This proof was found in "Basic Stochastic Processes by Zdzisław Brzeźniak & Tomasz Zastawniak(1999)".


## Algorithms


```{r, echo = TRUE, eval=FALSE}
set.seed(123)
```

```{r, echo = TRUE, eval=FALSE}
rounds = 250
initial_capital = 50
roullete_round<-sample(0:36, rounds, replace = T)
capital = matrix(0, 1, rounds + 1)
capital[1] = initial_capital
for(i in 1:rounds ){
  if(roullete_round[i] == 0){
    capital[i+1]=capital[i] + 35}
  else{
    capital[i+1]=capital[i] - 1
  }
}
x=seq(from=1, to=rounds + 1, by=1)
plot(x,capital,type="s", main="Graph of X_n ", xlab = "Rounds", ylab = "X_n", col = "red")
```

```{r, echo = TRUE, eval=FALSE}
#Function that simulates betting a unit on zero(every round) with specific initial capital and rounds to play. 
#The function roullete_sim_zero returns a vector, of which the i-th element is the capital at the i-th round.
#The function roullete_sim_zero_ruin calculates the round on which the gambler goes bust. If he does not do so, it returns the number of rounds.
roullete_sim_number<-function(rounds, initial_capital){
roullete_round<-sample(0:36, rounds, replace = T)
capital = matrix(0, 1, rounds + 1)
capital[1] = initial_capital
for(i in 1:rounds){
  if(roullete_round[i] == 0){
    capital[i+1]=capital[i] + 35}
  else{
    capital[i+1]=capital[i] - 1
  }
}
#x=seq(from=1, to=rounds + 1, by=1)
#plot(x,capital,type="s", main="Roullete", xlab = "Rounds", ylab = "Capital", col = "red")
return(capital)
}
roullete_sim_number_ruin<-function(rounds, initial_capital){
  
  roullete_round<-sample(0:36, rounds, replace = T)
  
  capital = matrix(0, 1, rounds + 1)
  capital[1] = initial_capital
  ruin_round=rounds
  flag=0
  
  i=1
  while(i < rounds + 1){ 
    if(roullete_round[i] == 0){
      capital[i+1]=capital[i] + 35}
    else{
      capital[i+1]=capital[i] - 1
    }
    if(capital[i+1] < 0){
      ruin_round=i
      i=rounds
    }
    i=i+1
  }
  return(ruin_round)
}
roullete_sim_number_exitcap<-function(rounds, initial_capital){
  roullete_round<-sample(0:36, rounds, replace = T)
  
  lcapital = 0
  capital=matrix(0,1, rounds + 1)
  capital[1]= initial_capital
  i=1
  flag=0
  
  while(flag == 0){
   if(roullete_round[i] == 0){
    capital[i+1]=capital[i] + 35}
   else{
    capital[i+1]=capital[i] - 1
   }
   if(capital[i+1] < 0){
     capital[i+1] = 0
     flag=1
   }
   if(i == rounds){
     flag = 1
   }
  i=i+1
  }  
  
  lcapital=capital[rounds+1]
  return(lcapital)
}
num_sim=10000
initial_capital=50
rounds=250
ruin_time<-function(num_sim,initial_capital,rounds){
  ruin_time=matrix(0,num_sim,1)
  for (i in 1:num_sim){
   ruin_time[i] = roullete_sim_number_ruin(rounds, initial_capital)
  }
  return(ruin_time)
}
  
ruin = ruin_time(num_sim, initial_capital, rounds)
sum_ruin_time=0
ruin_amount=0
for (i in 1:num_sim){
  sum_ruin_time = sum_ruin_time + ruin[i]
  if (ruin[i] != 250){
    ruin_amount = ruin_amount + 1
  }
}
mean_ruin_time= sum_ruin_time/num_sim
exiting_capital<-function(num_sim,initial_capital, rounds){
  exiting_capital=matrix(0,num_sim, 1)
  for (i in 1:num_sim){
    exiting_capital[i] = roullete_sim_number_exitcap(rounds, initial_capital)
  }
  return(exiting_capital)
}
exit_cap = exiting_capital(num_sim, initial_capital, rounds)
winner_percent=0
for (i in 1:num_sim){
  if(exit_cap[i] > initial_capital){
    winner_percent = winner_percent + 1
  }
}
loser_percent = num_sim - winner_percent
```


```{r, echo = TRUE, eval=FALSE}
breaks1 = seq(from = 0, to = rounds, by = 2)
hist(ruin, breaks1, main = "Ruin rounds when betting at a single number", xlab = "Rounds")
```


```{r, echo = TRUE, eval=FALSE}
breaks2 = seq(from = min(exit_cap), to = max(exit_cap) + 20, by = 10)
hist(exit_cap, breaks2, main = "Capital after all rounds", xlab = "Capital")
```
