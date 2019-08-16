# Biostats 213 Midterm Project-Part 3
# 3 tellers, 1 line
# Teia Noel

## INTENSITY FUNCTION GENERATION ## 
lambda.t <- function(time){		# the banks open from 8am to 5pm, so we set 	
  if(time >= 4 && time <= 5){	# the time from 0 to 9 and the rate is 4/hr 
    lambda <- 6		# but increase to 6 between 4 and 5.
  } else { lambda <- 4 }
  return(lambda)
}
## END INTENSITY FUNCTION GENERATION ##

## GLOBAL VARIABLE INTITIALIZATION ##
N<-100 # 100 runs of simulation
cust.wait<-numeric(N) # wait time for each simulation run
Tp<-0 # time past closing for each simulation run? 

Timepast<-numeric(N) 
i<-1 # what is difference between this and Tp?
## END GLOBAL VARIABLE INITIALIZATION ##

for (i in 1:N){	
  ## START VARIABLE INITIALIZATION ##
  t<-0 # time starts at 0
  Tmax<-9 # store closes after 9 hours
  A<-D<-c() # to store arrival and departure times
  Na<-0 # number of arrivals in store initialized to 0
  c1<-c2<-c3<-0 # number of customers served by all cashiers is 0
  SS<-c(0,0,0,0) # SS(n,i1,i2,i3); n is number of people in store, i1 is # customer at first cashier, i2 is that for second cashier, i3 is that for third cashier
  t<-t-(1/lambda.t(t))*log(runif(1)) # generate time of first arrival
  Ta<-T0<-t # next arrival
  Td1<-Td2<-Td3<-9999 # nobody in the store, so td is "infinity" for each teller
  bool<-TRUE
  ## END VARIABLE INITIALIZATION ##
  
  while(bool){
    next_event<-min(Ta,Td1,Td2,Td3,Tmax)
    ## START CASE 1 ##
    if(next_event==Ta){ # somebody arrives before anyone departs from the store and before closing
      t<-Ta # time axis moves forward by Ta
      Na<-Na+1 # one more person arrives
      Ta<-t-(1/lambda.t(t))*log(runif(1)) # generate new time of arrival
      A[Na]<-t # keep track of time of arrival
      if(SS[1]==0){ # nobody is in the store
        SS[1]<-1 # number of people in store is now 1
        SS[2]<-Na # Nath person is now being served by teller 1
        Y<-rgamma(1,1,10) # generate next departure and reset Td1
        Td1<-t+Y
      }
      else if(SS[1]==1){ # if there is one person in the store
        SS[1]<-2 # two people now in store
        if(SS[2] != 0){ # person is being served by teller 1
          SS[3]<-Na # new arrival served by server 2
          Y<-rgamma(1,1,10) # generate next departure and reset Td2
          Td2<-t+Y
        }
        else if(SS[3] != 0 | SS[4] != 0){ # person is being served by teller 2 or 3
          SS[2]<-Na # new arrival served by server 1
          Y<-rgamma(1,1,10) # generate next departure and reset Td1
          Td1<-t+Y
        }
      }
      else if(SS[1]==2){ # if there are two people in the store
        SS[1]<-3 # three people now in the store
        if(SS[2]==0){ # nobody being served by teller 1
          SS[2]<-Na # new arrival served by teller 1
          Y<-rgamma(1,1,10) # generate next departure and reset Td2
          Td1<-t+Y
        }
        else if(SS[3]==0){ # nobody being served by teller 2
          SS[3]<-Na # new arrival served by teller 2
          Y<-rgamma(1,1,10) # generate next departure and reset Td2
          Td2<-t+Y
        }
        else if(SS[4]==0){ # nobody being served by teller 3
          SS[4]<-Na # new arrival served by teller 3
          Y<-rgamma(1,1,10) # generate next departure and reset Td3
          Td3<-t+Y
        }
      }
      else if(SS[1]>2){ # there is a line
        SS[1]<-SS[1]+1 # increment number of people in store
      }
    }
    ## END CASE 1 ##
    
    ## START CASE 2 ##
    else if(next_event==Td1){ # somebody leaves from teller 1 before someone departs from other tellers, someone arrives, and closing time
      t<-Td1 # move time axis forward by Td1
      c1<-c1+1 # increment number of people served by first teller
      D[SS[2]]<-t # keep track of departure time of customer served by teller 1
      if(SS[1]==1){ # person leaving was only one left
        SS[1]<-0 # nobody in store
        SS[2]<-0 # person left server 1
        Td1<-9999
      }
      else if(SS[1]==2 | SS[1]==3){ # there are two or three people in the store
        SS[1]<-SS[1]-1 # decrement number of people in store
        SS[2]<-0 # person left server 1
        Td1<-9999
      }
      else if(SS[1]>3){ # there is a line
        SS[1]<-SS[1]-1 # decrement number of people in store
        m<-max(SS[2],SS[3],SS[4])
        SS[2]<-m+1 # next person to be served by teller 1
        Y<-rgamma(1,1,10) # generate next time of departure and reset Td1
        Td1<-t+Y
      }
      
    }
    ## END CASE 2 ##
    
    ## START CASE 3 ##
    else if(next_event==Td2){ # somebody leaves from teller 2 before someone departs elsewhere, someone arrives, and closing time
      t<-Td2 # move time axis forward by Td2
      c2<-c2+1 # increment number of people served by second teller
      D[SS[3]]<-t # keep track of departure time for customer served by teller 2
      if(SS[1]==1){ # one person in the store
        SS[1]<-0 # nobody left in store
        SS[3]<-0 # person left server 2
        Td2<-9999
      }
      else if(SS[1]==2 | SS[1]==3){ # number of people in the store is between 2 and 3
        SS[1]<-SS[1]-1 # decrement number of people in store
        SS[3]<-0 # person left server 2
        Td2<-9999
      }
      else if(SS[1]>3){ # there is a line
        SS[1]<-SS[1]-1
        m<-max(SS[2],SS[3],SS[4])
        SS[3]<-m+1 # next person to be served by teller 2
        Y<-rgamma(1,1,10) # generate next time of departure and reset Td2
        Td2<-t+Y
      }
    }
    ## END CASE 3 ##
    
    ## START CASE 4 ##
    else if(next_event==Td3){ # someone leaves teller 3 before someone departs elsewhere, someone arrives, and closing time
      t<-Td3 # move time axis forward by Td3
      c3<-c3+1 # increment number of people served by teller 3
      D[SS[4]]<-t # keep track of departure time for customer served by teller 3
      if(SS[1]==1){ # one person in the store
        SS[1]<-0 # decrement number of people in the store
        SS[4]<-0 # person left server 3
        Td3<-9999 
      }
      else if(SS[1]==2 | SS[1]==3){ # there are between 2 and 3 people in the store
        SS[1]<-SS[1]-1 # decrement number of people in the store
        SS[4]<-0 # person left teller 3
        Td3<-9999
      }
      else if(SS[1]>3){ # there is a line
        SS[1]<-SS[1]-1 # decrememnt number of people in the store
        m<-max(SS[2],SS[3],SS[4]) 
        SS[4]<-m+1 # next person to be served by teller 3
        Y<-rgamma(1,1,10) # generate departure time and update Td3
        Td3<-t+Y
      }
    }
    ## END CASE 4 ##
    
    ## START CASE 5 ##
    else if(next_event==Tmax){
      j<-1
      while(SS[1]>0){
        t<-min(Td1,Td2,Td3) # move time axis forward by minimum departure time
        if(t==Td1){
          c1<-c1+1 # increment number of people served by first teller
          D[SS[2]]<-t # keep track of departure time of customer served by teller 1
          if(SS[1]==1){ # person leaving was only one left
            SS[1]<-0 # nobody in store
            SS[2]<-0 # person left server 1
            Td1<-9999
          }
          else if(SS[1]==2 | SS[1]==3){ # there are two or three people in the store
            SS[1]<-SS[1]-1 # decrement number of people in store
            SS[2]<-0 # person left server 1
            Td1<-9999
          }
          else if(SS[1]>3){ # there is a line
            SS[1]<-SS[1]-1 # decrement number of people in store
            m<-max(SS[2],SS[3],SS[4])
            SS[2]<-m+1 # next person to be served by teller 1
            Y<-rgamma(1,1,10) # generate next time of departure and reset Td1
            Td1<-t+Y
          }
        }
        else if(t==Td2){
          c2<-c2+1 # increment number of people served by second teller
          D[SS[3]]<-t # keep track of departure time for customer served by teller 2
          if(SS[1]==1){ # one person in the store
            SS[1]<-0 # nobody left in store
            SS[3]<-0 # person left server 2
            Td2<-9999
          }
          else if(SS[1]==2 | SS[1]==3){ # number of people in the store is between 2 and 3
            SS[1]<-SS[1]-1 # decrement number of people in store
            SS[3]<-0 # person left server 2
            Td2<-9999
          }
          else if(SS[1]>3){ # there is a line
            SS[1]<-SS[1]-1
            m<-max(SS[2],SS[3],SS[4])
            SS[3]<-m+1 # next person to be served by teller 2
            Y<-rgamma(1,1,10) # generate next time of departure and reset Td2
            Td2<-t+Y
          }
        }
        else if(t==Td3){
          c3<-c3+1 # increment number of people served by teller 3
          D[SS[4]]<-t # keep track of departure time for customer served by teller 3
          if(SS[1]==1){ # one person in the store
            SS[1]<-0 # decrement number of people in the store
            SS[4]<-0 # person left server 3
            Td3<-9999 
          }
          else if(SS[1]==2 | SS[1]==3){ # there are between 2 and 3 people in the store
            SS[1]<-SS[1]-1 # decrement number of people in the store
            SS[4]<-0 # person left teller 3
            Td3<-9999
          }
          else if(SS[1]>3){ # there is a line
            SS[1]<-SS[1]-1 # decrememnt number of people in the store
            m<-max(SS[2],SS[3],SS[4]) 
            SS[4]<-m+1 # next person to be served by teller 3
            Y<-rgamma(1,1,10) # generate departure time and update Td3
            Td3<-t+Y
          }
        }
      }
      if(SS[1]==0){
        Tp<-max(t-Tmax,0) # calculate time past closing
        Timepast[i]<-Tp # store in timepast array for this iteration
        bool<-FALSE # end simulation once store has closed and no one left 
      }
    }
    ## END CASE 5 ##
  }
  cust.wait[i]<-sum(D-A)/Na # store avg wait time for one simulation
}
cat("Average customer wait time: ", mean(cust.wait))

