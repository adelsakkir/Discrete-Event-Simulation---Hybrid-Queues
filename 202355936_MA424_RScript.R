#Candidate Number - 202355936
# This R script is divided as follows 
# 1) generation of inter arrival times
# 2) generation of service times
# 3) Simulation Functions
# 4) Running the simluation for K iteration 
# 5) Several calculations for questions in the report

set.seed(1)
# Function to generate arrival time
generate_arrival <- function(lambda) {
  U1 = runif(1)
  t = - (1/lambda) * log(U1)
  return(t)
}

# Function to generate service time
generate_service_time <- function(mu) {
  U1 = runif(1)
  t = - (1/mu) * log(U1)
  return(t)
}

simulation <- function(){
  # Initialization
  t <- 0
  Na <- 0
  nSC <- 0
  nSF <- 0
  nSW <- 0
  NSC <- 0
  NSF <- 0
  NSW <- 0
  Ca <- 0
  Cr <- 0
  NACS <- 0
  ST <- matrix(,ncol = 3, nrow = 10000, dimnames = list(NULL, c("nSF", "nSC", "nSW")))
  Event_List <- matrix(,  ncol = 4, dimnames = list(NULL, c("tA", "tSC", "tSF", "tSW")))
  A <- c()
  D <- rep(0,100000)
  accepted <- c()
  rejected <- c()
  
  #Parameters
  lambda = 8
  muC  = 5
  muF = 6
  muW = 4
  p = 0.5
  q = 0.6
  r = 0.4
  T <- 8 #8 hours of working
  
  tA = generate_arrival(lambda)
  tSC = Inf
  tSF = Inf
  tSW = Inf
  flag <- 0
  
  
  #Main simulation loop
  while (flag == 0) {
    #  CASE 1 - Arrival
    if (min(tA, tSC, tSF, tSW,T) == tA){
    # CASE 1 - Arrival
    # print ("Case 1 - Arrival")
    t <- tA
    tA <- t + generate_arrival(lambda)
    Na <- Na + 1
    A <- c(A, t)
    
    # move to SF
    if (runif(1) <= p) {
      nSF <- nSF + 1
      
      NSF <- NSF + 1
      NACS <- NACS + 1
      ST[Na, "nSF"] <- nSF
      
      if (nSF == 1) {
        tSF <- t + generate_service_time(muF)
      }
    } 
    # move to SC
    else {
      nSC <- nSC + 1

      NSC <- NSC + 1
      ST[Na, "nSC"] <- nSC
      if (nSC == 1) {
        tSC <- t + generate_service_time(muC)
      }
    }
  
    Event_List <- rbind(Event_List, c(tA, tSC, tSF, tSW))
  
    
    } 
    # CASE 2 - Departure at SC
    else if (min(tA, tSC, tSF, tSW,T)==tSC) {
      "Case 2 - Departure at SC"
      t <- tSC
      person_dept <- which(ST[, "nSC"] == 1)
      nSC <- nSC - 1
      # print(ST)
      ST[, "nSC"] <- pmax(0, ST[, "nSC"] - 1)
      # print(ST)
      # print(paste0("Person departing from SC ", person_dept))
      
      
  
      if (runif(1) < q) {
        # print(paste0("Person going to SF :", person_dept))
        nSF <- nSF + 1
        NSF <- NSF + 1
        ST[person_dept, "nSF"] <- nSF #max(ST[, "nSF"]) + 1
        # print(ST)
        if (nSF ==1) {tSF <- t + generate_service_time(muF)}
        # break
        
      } else {
        # print(paste0("Person going to SW:", person_dept))
        nSW <- nSW + 1
        
        NSW <- NSW + 1
        ST[person_dept, "nSW"] <- nSW #max(ST[, "nSW"]) + 1
        if (nSW ==1) {tSW <- t + generate_service_time(muW)}
        # break
      }
  
      if (nSC == 0) {
        tSC <- Inf
      } else {
        tSC <- t + generate_service_time(muC)
      }
  
      Event_List <- rbind(Event_List, c(tA, tSC, tSF, tSW))
    }
  
    # CASE 3 - Departure at SF
    else if (min(tA, tSC, tSF, tSW,T) == tSF) {
      # print ("CASE 3 - Departure at SF (Application Accepted))")
      t <- tSF
      person_dept <- which(ST[, "nSF"] == 1)
      nSF <- nSF - 1
      
      ST[, "nSF"] <- pmax(0, ST[, "nSF"] - 1)
      Ca <- Ca + 1
      D[person_dept] <- tSF
      accepted <- c(accepted, person_dept) #Recording who got accepted
      
  
      if (nSF == 0) {
        tSF <- Inf
      } else {
        tSF <- t + generate_service_time(muF)
      }
  
      Event_List <- rbind(Event_List, c(tA, tSC, tSF, tSW))
    }
  
    # CASE 4 - Departure at SW
    else if (min(tA, tSC, tSF, tSW,T) == tSW) {
      # print ("CASE 4 - Departure at SW (Application Rejected))")
      t <- tSW
      person_dept <- which(ST[, "nSW"] == 1)
      nSW <- nSW - 1
      
      ST[, "nSW"] <- pmax(0, ST[, "nSW"] - 1)
  
      if (runif(1) <= r) {
        nSF <- nSF + 1
        NSF <- NSF + 1
        ST[person_dept, "nSF"] <- nSF #max(ST[, "nSF"]) + 1
        if (nSF ==1) {tSF <- t + generate_service_time(muF)}
      } else {
        Cr <- Cr + 1
        D[person_dept] <- tSW
        rejected <- c(rejected, person_dept)
      }
  
      if (nSW == 0) {
        tSW <- Inf
      } else {
        tSW <- t + generate_service_time(muW)
      }
  
      Event_List <- rbind(Event_List, c(tA, tSC, tSF, tSW))
    }
  
    # CASE 5 - Simulation Ends
    else if (min(tA, tSC, tSF, tSW,T) == T){
      # print ("CASE 5 - Time Up")
      if (tSC == Inf && tSF == Inf && tSW == Inf) {
        # print("Simulation Over")
      Tp <- max(t - T, 0)
      flag <-1
      break
      
    } else if (min(tSC, tSF, tSW) == tSC) {
      "Case 2 - Departure at SC"
      t <- tSC
      nSC <- nSC - 1
      person_dept <- which(ST[, "nSC"] == 1)
      ST[, "nSC"] <- pmax(0, ST[, "nSC"] - 1)
      
      if (runif(1) < q) {
        nSF <- nSF + 1
        NSF <- NSF + 1
        ST[person_dept, "nSF"] <- nSF #max(ST[, "nSF"]) + 1
        if (nSF ==1) {tSF <- t + generate_service_time(muF)}
        
      } else {
        nSW <- nSW + 1
        NSW <- NSW + 1
        ST[person_dept, "nSW"] <- nSW #max(ST[, "nSW"]) + 1
        if (nSW ==1) {tSW <- t + generate_service_time(muW)}
      }
      
      if (nSC == 0) {
        tSC <- Inf
      } else {
        tSC <- t + generate_service_time(muC)
      }
      
      Event_List <- rbind(Event_List, c(tA, tSC, tSF, tSW))
      
    
      } # Continue with CASE 2
      else if (min(tSC, tSF, tSW) == tSF) {
      # Continue with CASE 3
        # print ("CASE 3 - Departure at SF (Application Accepted))")
        t <- tSF
        person_dept <- which(ST[, "nSF"] == 1)
        nSF <- nSF - 1
        
        ST[, "nSF"] <- pmax(0, ST[, "nSF"] - 1)
        Ca <- Ca + 1
        D[person_dept] <- tSF
        accepted <- c(accepted, person_dept)
        
        
        if (nSF == 0) {
          tSF <- Inf
        } else {
          tSF <- t + generate_service_time(muF)
        }
        
        Event_List <- rbind(Event_List, c(tA, tSC, tSF, tSW))
      
    } # Continue with CASE 3
      else if (min(tSC, tSF, tSW) == tSW) {
        # print ("CASE 4 - Departure at SW (Application Rejected))")
        t <- tSW
        person_dept <- which(ST[, "nSW"] == 1)
        nSW <- nSW - 1
        ST[, "nSW"] <- pmax(0, ST[, "nSW"] - 1)
        #Ca <- Ca + 1
        
        if (runif(1) <= r) {
          nSF <- nSF + 1
          NSF <- NSF + 1
          ST[person_dept, "nSF"] <- nSF #max(ST[, "nSF"]) + 1
          if (nSF ==1) {tSF <- t + generate_service_time(muF)}
        } else {
          Cr <- Cr + 1
          D[person_dept] <- tSW
          rejected <- c(rejected, person_dept)
        }
        
        if (nSW == 0) {
          tSW <- Inf
        } else {
          tSW <- t + generate_service_time(muW)
        }
        
        Event_List <- rbind(Event_List, c(tA, tSC, tSF, tSW))
    } # Continue with CASE 4
    }
  }
  

  Ta <- head(D-A,length(A))
  Ta_avg <- mean(Ta)
  accepted <- sort(accepted)
  rejected <- sort(rejected)
  
  # Storing time spend by completed (accepted) and rejected customers
  Tc <- c()
  Tr <- c()
  for (x in 1:Na) {
    if (x %in% accepted){
      Tc <- c(Tc,D[x]-A[x])
    }
    else {
      Tr <- c(Tr,D[x]-A[x])
    }
  }
  Tc_avg <- mean(Tc)
  Tr_avg <- mean(Tr)
  no_accepted <-  length(accepted)
  no_rejected <-  length(rejected)
  
  return (c(Ta_avg, Tc_avg, Tr_avg, no_accepted, no_rejected))

}

# Running the Simulations

K <-  80
Ta_avg_v <- c() #A vector to store average time spent by customer
Tc_avg_v <- c() #A vector to store average time spent by completed customer
Tr_avg_v <- c() #A vector to store average time spent by rejected customer
no_accepted_v <- c()
no_rejected_v <- c()
percent_comp <- c()

for (x in 1:K) {
  listi <- simulation()
  # print(paste("hi",list[1]))
  Ta_avg_v <- c(Ta_avg_v, listi[1])
  Tc_avg_v <- c(Tc_avg_v, listi[2])
  Tr_avg_v <- c(Tr_avg_v, listi[3])
  no_accepted_v <- c(no_accepted_v, listi[4])
  no_rejected_v <- c(no_rejected_v, listi[5])
  percent_comp <- c(percent_comp, listi[4]/(listi[4]+listi[5]))
}

# Calculations based on the simulation run for the report. 
mean_Ta <- mean(Ta_avg_v)#*60 #converting to minutes
mean_Tc <- mean(Tc_avg_v)#*60 #converting to minutes
mean_Tr <- mean(Tr_avg_v)#*60 #converting to minutes
mean_percent_comp <- mean(percent_comp)
print(paste((mean_Ta - (1.644853627*(sd(Ta_avg_v)/sqrt(K))))*60,(mean_Ta + (1.644853627*(sd(Ta_avg_v)/sqrt(K))))*60))
print(mean_Ta*60)
print(mean_Tc*60)
print(mean_Tr*60)
print((1.96*(sd(Ta_avg_v)/sqrt(K)))*60)
print((1.96*(sd(Tc_avg_v)/sqrt(K)))*60)
print((1.96*(sd(Tr_avg_v)/sqrt(K)))*60)
target_sample_sd <- (1/6)/1.96
print(paste("target",target_sample_sd))
(sd(Ta_avg_v)/sqrt(K))*60
mean_percent_comp
print(paste((mean_percent_comp - (1.96*(sd(percent_comp)/sqrt(K)))),(mean_percent_comp + (1.96*(sd(percent_comp)/sqrt(K))))))
