# Biostats 213 Midterm Project-Part 1
# 3 tellers, 3 lines

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
  D1<-D2<-D3<-c() # vector of departures for each teller
  
  Na1<-Na2<-Na3<-0 # number of arrivals by t for each teller
  Nd1<-Nd2<-Nd3<-0 # number of departures by t for each teller
  n<-n1<-n2<-n3<-0 # number of people in line for each teller
  Td1<-Td2<-Td3<-9999 # nobody in the store, so td is "infinity" for each teller
  Ta1<-Ta2<-Ta3<-0 # arrival times for each teller
  Ta<-0 # arrival time in store
  t<-t-(1/lambda.t(t))*log(runif(1)) # generate the first arrival time with HPP
  Ta<-T0<-t # next arrival
  bool<-TRUE
  ## END VARIABLE INITIALIZATION ##
  
  while(bool){
    next_event <- min(Ta,Td1,Td2,Td3,Tmax)
    
    ## START CASE 1 ##
    if(next_event == Ta){ # arrival occurs before any departure and closing time
      t<-Ta # move time axis forward by Ta
      min_line<-min(n1,n2,n3) # calculate shortest line
      
      if(min_line == n1){ # shortest line is line 1
        Na1<-Na1+1 # increment number of people in line 1
        n1<-n1+1 # add additional customer to line 1
        A1[Na1]<-t # keep track of arrival time in line 1
      
        Ta<-t-(1/lambda.t(t))*log(runif(1)) # generate new arrival time
        
        if (n1==1){ # if n1=1 we need to generate next departure time and reset Td1
          Y <- rgamma(1,1,10)
          Td1 <- t + Y
        }	
      }
      else if(min_line == n2){ # shortest line is line 2
        Na2<-Na2+1 # increment number of arrivals in line 2
        n2<-n2+1 # increment number of people in line 2
        A2[Na2]<-t # keep track of arrival time in line 2
        
        Ta<-t-(1/lambda.t(t))*log(runif(1)) # generate new arrival time
        
        if (n2==1){ # if n2=1 we need to generate next departure time and reset Td
          Y <- rgamma(1,1,10)
          Td2 <- t + Y
        }	
        
      }
      else if(min_line == n3){ # n3 is shortest line
        Na3<-Na3+1 # update variables for line 3 as was done for lines 1 and 2
        n3<-n3+1 
        A3[Na3]<-t
        
        Ta<-t-(1/lambda.t(t))*log(runif(1))
        
        if (n3==1){ 
          Y <- rgamma(1,1,10)
          Td3 <- t + Y
        }	
        
      }
    }
    ## END CASE 1 ##
    
    ## START CASE 2 ##
    else{
      # handle departures
      if(next_event == Td1){ # departure from line 1 occurs before any other departure, arrival, and Tmax
        t<-Td1 # move time axis forward by Td1
        Nd1<-Nd1+1 # add additional departure from line 1
        n1<-n1-1 # decrement number of customers in line 1
        D1[Nd1]<-t # customer Nd1 departed at time t
        if (n1==0) { # if there are no customers in line 1, td1 is infinity
          Td1<-99999 
        }
        if (n1 > 0){	# if there are customers in line 1, generate next departure time and reset Td1
          Y<-rgamma(1,1,10)
          Td1<-t+Y
        }
      }
      else if(next_event == Td2){ # departure from line 2 occurs before any other departure, arrival, and Tmax
        t<-Td2 # update variables for line 2 as was done for line 1
        Nd2<-Nd2+1 
        n2<-n2-1 
        D2[Nd2]<-t 
        if (n2==0) { 
          Td2<-99999 
        }
        if (n2 > 0){	
          Y<-rgamma(1,1,10)
          Td2<-t+Y
        }
      }
      else if(next_event == Td3){ # departure from line 3 occurs before any other departure, arrival, and Tmax
        t<-Td3 # updating variables for line 3 as was done for lines 1 and 2
        Nd3<-Nd3+1 
        n3<-n3-1 
        D3[Nd3]<-t 
        if (n3==0) { 
          Td3<-99999 
        }
        if (n3 > 0){	
          Y<-rgamma(1,1,10)
          Td3<-t+Y
        }
      }
    }
    ## END CASE 2 ##
    
    ## START CASE 3 ##
    if(next_event == Tmax){
      while (n1+n2+n3>0){ # store is not empty; empty out store
        t<-min(Td1,Td2,Td3) # move time axis forward by first departure
        
        if(t == Td1){ # departure from line 1
          Nd1<-Nd1+1 # add additional departure from line 1
          n1<-n1-1 # decrement total customers in line 1
          D1[Nd1]<-t # customer Nd1 departed at time t
          if(n1==0){ # no one left in line 1
            Td1<-9999 # Td1 is infinity
          }
          if (n1>0){ # as long as customers still remain in line 1 after departure
            Y<-rgamma(1,1,10) # generate next departure time and reset Td1
            Td1<-t+Y
          }
        }
        else if(t == Td2){ # departure from line 2
          Nd2<-Nd2+1 # update variables for line 2 as was done for line 1
          n2<-n2-1 
          D2[Nd2]<-t 
          if(n2==0){
            Td2<-9999
          }
          if (n2>0){ 
            Y<-rgamma(1,1,10) 
            Td2<-t+Y
          }
        }
        else if(t == Td3){ # departure from line 3
          Nd3<-Nd3+1 # update variables for line 3 as was done for lines 1 and 2
          n3<-n3-1 
          D3[Nd3]<-t 
          if(n3==0){
            Td3<-9999
          }
          if (n3>0){ 
            Y<-rgamma(1,1,10) 
            Td3<-t+Y
          }
        }
      }
      if(n1+n2+n3 == 0){ # no one left in store
        Tp<-max(t-Tmax,0) # calculate time past closing
        Timepast[i]<-Tp # store in timepast array for this iteration
        bool<-FALSE # end simulation once store is empty and past closing
      }
    }
    ## END CASE 3 ##
  }
  A<-c(A1,A2,A3)
  D<-c(D1,D2,D3)
  Na<-Na1+Na2+Na3
  cust.wait[i]<-sum(D-A)/Na # store wait times for every simulation
}
cat("Average wait time is: ", mean(cust.wait))