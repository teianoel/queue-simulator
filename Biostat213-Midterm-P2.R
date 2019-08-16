# Biostats 213 Midterm Project-Part 2
# 3 tellers in tandem
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
  A1<-A2<-A3<-c() # vector of arrivals for each teller
  D<-c() # vector of departures for each teller
  Na1<-Na2<-Na3<-0 # number of arrivals by t for each teller
  Nd<-0 # number of departures by t for each teller
  n1<-n2<-n3<-0 # number of people in line for each teller
  Td1<-Td2<-Td3<-9999 # nobody in the store, so td is "infinity" for each teller
  Ta<-0 # arrival time in store
  t<-t-(1/lambda.t(t))*log(runif(1)) # generate the first arrival time with HPP
  Ta<-T0<-t # next arrival
  bool<-TRUE
  ## END VARIABLE INITIALIZATION ##
  
  while(bool){
    next_event<-min(Ta,Td1,Td2,Td3,Tmax) 
    
    ## START CASE 1 ##
    if(next_event==Ta){ # somebody arrives before anyone departs from the store and before closing
      t<-Ta # move time axis forward by arrival time
      Na1<-Na1+1 # increment number of arrivals in line for teller 1
      n1<-n1+1 # increment number of people in line for teller 1
      Ta<-t-(1/lambda.t(t))*log(runif(1)) # generate new arrival time
      A1[Na1]<-t # keep track of time of arrival in line for teller 1
      
      if(n1==1){ # if there is one person in line for teller 1
        Y<-rgamma(1,1,10) # generate departure time and reset Td1
        Td1<-t+Y
      }
    }
    ## END CASE 1 ##
    
    ## START CASE 2 ##
    else if(next_event==Td1){ # somebody leaves from teller 1 before someone departs from other tellers, someone arrives, and closing time
      t<-Td1 # move time axis forward by Td1
      n1<-n1-1 # decrement number of people in line for teller 1 
      n2<-n2+1 # increment number of people fed into line for teller 2
      Na2<-Na2+1 # increment number of arrivals in line for teller 2
      A2[Na2]<-t # keep track of arrival time in line 2
      
      if(n1==0){ # if nobody is left in the store
        Td1<-9999 # set Td1 to infinity
      }
      else if(n1>0){ # otherwise generate new arrival time and reset Td1
        Y<-rgamma(1,1,10)
        Td1<-t+Y
      }
      if(n2==1){ # if number of people in second line is now 1
        Y<-rgamma(1,1,10) # generate departure time and reset Td2
        Td2<-t+Y
      }
    }
    ## END CASE 2 ##
    
    ## START CASE 3 ##
    else if(next_event==Td2){ # somebody leaves from teller 2 before someone departs elsewhere, someone arrives, and closing time
      t<-Td2 # move time axis forward by Td2
      n2<-n2-1 # decrement number of people in line 2
      n3<-n3+1 # increment number of people fed into line 3
      Na3<-Na3+1 # increment number of arrivals in line 3
      A3[Na3]<-t # keep track of time of arrival in line  3
      
      if(n2==0){ # if there are no people in line 2
        Td2<-9999 # set Td2 to infinity
      }
      else if(n2>0){ # otherwise, if there are people in line 2
        Y<-rgamma(1,1,10) # generate departure time and reset Td2
        Td2<-t+Y
      }
      
      if(n3==1){ # if there is only one person in line 3
        Y<-rgamma(1,1,10) # generate departure time and reset Td3
        Td3<-t+Y
      }
    }
    ## END CASE 3 ##
    
    ## START CASE 4 ##
    else if(next_event==Td3){ # if departure from line 3 happens before any other departure, arrival, and Tmax
      t<-Td3 # move time axis forward by Td3
      Nd<-Nd+1 # incrememnt number of departures by 1 (last server in tandem service)
      n3<-n3-1 # decrement number of people in line 3
      D[Nd]<-t # keep track of time of departure
      
      if(n3==0){ # if there is no one left in line 3
        Td3<-9999 # set Td3 to infinity
      }
      else if(n3>0){ # if there are people left in line 3
        Y<-rgamma(1,1,10) # generate new departure time and reset Td3
        Td3<-t+Y
      }
    }
    ## END CASE 4 ##
    
    ## START CASE 5 ##
    else if(next_event==Tmax){ # if the store closes before any arrival or departure
      while(n1+n2+n3>0){ # as long as there are people in the store
        t<-min(Td1,Td2,Td3) # move time axis forward by earliest departure time
        if(t==Td1){ # if departure occurs from line 1
          n1<-n1-1 # decrement number of people from line 1
          n2<-n2+1 # increment number of people in line 2
          Na2<-Na2+1 # incrememnt number of arrivals in line 2
          A2[Na2]<-t # keep track of arrival time in line 2 
          
          if(n1==0){ # if there is no one left in line 1
            Td1<-9999 # set Td1 to infinity
          }
          else if(n1>0){ # if there are people in line 1
            Y<-rgamma(1,1,10) # generate new departure time and reset Td1
            Td1<-t+Y
          }
          
          if(n2==1){ # if there is one person in line 2
            Y<-rgamma(1,1,10) # generate new departure time and rest Td2
            Td2<-t+Y
          }
        }
        else if(t==Td2){ # departure occurs from line 2 
          n2<-n2-1 # decrememnt number of people in line 2
          n3<-n3+1 # incrememnt number of people in line 3
          Na3<-Na3+1 # increment number of arrivals in line 3
          A3[Na3]<-t # keep track of time of arrival in line 3
          
          if(n2==0){ # if no one is left in line 2
            Td2<-9999 # reset Td2 to infinity
          }
          else if(n2>0){ # if there are people left in line 2
            Y<-rgamma(1,1,10) # generate new departure time and reset Td2
            Td2<-t+Y
          }
          
          if(n3==1){ # if there is one person left in line 3
            Y<-rgamma(1,1,10) # generate new departure time and reset Td3
            Td3<-t+Y
          }
        }
        else if(t==Td3){ # next departure is from line 3
          Nd<-Nd+1 # incrememnt number of departures
          n3<-n3-1 # decrement number of people in line 3
          D[Nd]<-t  # keep track of departure time
          
          if(n3==0){ # if no one is left in line 3
            Td3<-9999 # reset Td3 to infinity
          }
          else if(n3>0){ # if there are people left in line 3
            Y<-rgamma(1,1,10) # generate departure time and reset Td3
            Td3<-t+Y
          }
        }
      }
      if(n1+n2+n3==0){
        Tp<-max(t-Tmax,0) # calculate time past closing
        Timepast[i]<-Tp # store in timepast array for this iteration
        bool<-FALSE # end simulation once store has closed and no one left
      }
    }
    ## END CASE 5 ##
  }
  cust.wait[i]<-sum(D-A1)/Na1 # store avg wait time for one simulation
}
cat("Average wait time: ", mean(cust.wait))
