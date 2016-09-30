numberOfStates <- 40

#'Priority-Queue. Code from Rosettacode, modified for Project
#'insert( priority, value): Takes a priority and a value(vector,list,..) as parameters. Inserts them to an
#'index in the queue according to the priority
#'pop(): Removes and returns the element with the highest priority (smallest numbers first) from the queue
#'isEmpty(): Returns TRUE if Queue is empty, FALSE otherwise
PriorityQueue <- function() {
  keys <- values <- NULL
  insert <- function(key, value) {
    insert.order <- findInterval(key, keys) #'Searches for the right index to put key in
    keys <<- append(keys, key, insert.order) #'Insert key in the right index in the list
    values <<- append(values, list(value), insert.order)
  }
  pop <- function() {
    head <- values[[1]]
    values <<- values[-1]
    keys <<- keys[-1]
    return(head)
  }
  isEmpty <- function() length(keys) == 0
  environment()
}

#'The traceback function. Takes the cameFrom-Matrix and the goal node. Traces back the path to the start.
#'Returns the path as a list of points.
traceback <- function(cameFrom, current){
  nextPoint <- current
  path <- list(nextPoint)
  while (!cameFrom[nextPoint] == 0) {
    nextPoint <- cameFrom[[nextPoint]]
    path <- append(path, list(nextPoint))
  }
  return(rev(path)) #' Reverses the order of the list, so it starts at the starting point
}


# A*-algorithm, croc version
# heuristic cost function is currently not implemented
astar <- function(a, b, edges){
  
  visitedNodes <- list()
  
  nodesFrontier <- PriorityQueue()
  
  # Save the last states
  cameFrom <- matrix(0, nrow = 1, ncol = numberOfStates)
  
  #TODO: convert that to list insted of matrix
  gScore <- matrix(Inf, nrow = 1, ncol = numberOfStates)
  gScore[1, a] <- 0
  
  #TODO: convert that to list insted of matrix
  fScore <- matrix(Inf, nrow = 1, ncol = numberOfStates)
  fScore[1, a] <- 0
  
  nodesFrontier$insert(fScore[1, a], a)
  
  #dim <- max(dim(hroads)) #'The dimension of the road matrix is needed to prevent going out of bounds
  
  while (!nodesFrontier$isEmpty()){
    current <- nodesFrontier$pop()
    # Check if goal is reached
    if (all(current == b)){
      #'Check whether start point is goal point. Only return this point, because traceback requires
      #'at least one step
      if (all(a==b)) {
        return(list(a,a))
      }
      else {
        return (traceback(cameFrom, current))
      }
    }
    # current already popped from queue
    # add current to visited nodes list
    visitedNodes <- append(visitedNodes, list(current))
    
    # then look at all neighbors of current
    neighbors <- getOptions(current, edges)
    for (neighbor in neighbors){
        
        # If neighbor already expanded, dont visit it again
        if (Position(function(x) identical(x, neighbor), visitedNodes, nomatch = 0) <= 0){
          # Calculate the tentative score (all roads are equal, so simply add 1)
          tentScore <- gScore[1, current] + 1
          
          #' If node is not in the frontier or a better path to a node is discovered, store values and add
          #' to the frontier queue
          if (Position(function(x) identical(x, neighbor), nodesFrontier$values, nomatch = 0) <= 0
              || tentScore < gScore[1, neighbor]){
            cameFrom[neighbor] <- list(current)
            gScore[1, neighbor] <- tentScore
            fScore[1, neighbor] <- gScore[1, neighbor] 
              #heuristic_cost(neighbor[1], neighbor[2], bx, by) --> currently no heuristic cost implemented
            nodesFrontier$insert(fScore[1, neighbor], neighbor)
          }
        }
    }
  }
  return (0)
}

# The function to be passed to runWheresCroc()
# It uses an HMM model to guess the position of croc
# and an A*-algorithm to find the shortest path to it
hmmWC=function(moveInfo,readings,positions,edges,probs) {
  
  # Make Transission probability matrix
  transMat = matrix(0, nrow = numberOfStates, ncol = numberOfStates)
  for (i in 1:numberOfStates){
    options=getOptions(i, edges)
    for (elem in options){
      transMat[i, elem] <- (1/length(options)) # Equal probability for all options
    }
  }

  # Get the sequence from $mem
  sequence <- moveInfo$mem
  
  crocsPosition <- 0
  
  touristEaten <- F
  
  # Check if a tourist has been eaten
  if (!is.na(positions[1])){
    if (positions[1] < 0){
      touristEaten <- T
    print("Tourist 1 has been eaten")
    currentPosition <- abs(positions[1])
    crocsPosition <- currentPosition
    # Set the readings to the exact mean values of the waterhole
    readings[1] <- probs[[1]][currentPosition,1]
    readings[2] <- probs[[2]][currentPosition,1]
    readings[3] <- probs[[3]][currentPosition,1]
    
    # Reset sequence
    #sequence <- list()
    #sequence[[1]] <- readings
    }
  } 
    # same for tourist 2
    if (!is.na(positions[2])){
      if (positions[2] < 0){
        touristEaten <- T
        print("Tourist 2 has been eaten")
      currentPosition <- abs(positions[2])
      crocsPosition <- currentPosition
      readings[1] <- probs[[1]][currentPosition,1]
      readings[2] <- probs[[2]][currentPosition,1]
      readings[3] <- probs[[3]][currentPosition,1]
      
      # Reset sequence
      #sequence <- list()
      #sequence[[1]] <- readings
      
      }
    }
  
  len <- length(sequence)
  sequence[[len+1]] <- readings
  
  if (!touristEaten){
    # Add current readings
    #len <- length(sequence)
    #sequence[[len+1]] <- readings
    # Calculate the most likely position of croc
    crocsPosition <- runViterbi(sequence = sequence, transmission = transMat, probs = probs)
  }
  
  
  # Calculate the most likely position of croc
  #print("Crocs position: ")
  #print(crocsPosition)
  
  # Find shortest path to Croc and go in that direction
  path <- astar(positions[3], crocsPosition, edges)
  move1 <- path[[2]]
  # If croc is far away, don't search at waterwhole
  options <- getOptions(positions[3], edges)
  # Check if croc is in proximity
  if (crocsPosition %in% options){
    move2 <- 0
  } else {
    # if not, move 2 steps
    move2 <- path[[3]]
  }
  
  
  moveInfo$moves=c(move1,move2)
  moveInfo$mem=sequence
  return(moveInfo)
}

# Calculate the probability that a reading comes from
# certain waterhole given the mean and standard deviation (sd)
# of that waterhole
calculateReadingProbability=function(reading, mean, sd) {
  # initialize probability with 0
  prob <- 0
  # location of crocodile known
  if (reading == mean){
    prob <- 1
  } else {
    # 1 sd interval --> 0.68
    if (reading >= (mean - sd) && reading <= (mean + sd)){
      prob <- 0.68
    } else {
      # 2 sd intervals --> 0.27
      if (reading >= (mean - 2*sd) && reading <= (mean + 2*sd)){
        prob <- 0.27
      } else {
        # more than 2 sd intervals --> 0.01
        prob <- 0.01
        #}
      }
    }
  }
  return(prob)
}

# Calculate the emission probability for a certain waterhole (wh)
# and the given values for salinity (sal), phosphate (phos) and nitrogen (nit)
# probs holds the matrices with the mean and standard deviations for each waterhole
calculateEmissionProbability=function(readings, wh, probs){
  
  # Calculate single probabilites for each reading
  salProb = calculateReadingProbability(readings[1], probs[[1]][wh,1], probs[[1]][wh,2])
  phosProb = calculateReadingProbability(readings[2], probs[[2]][wh,1], probs[[2]][wh,2])
  nitProb = calculateReadingProbability(readings[3], probs[[3]][wh,1], probs[[3]][wh,2])
  
  # Return joint probability of these readings for wh
  #return (salProb * phosProb * nitProb)
  # Add the logarithms
  return(log(salProb) + log(phosProb) + log(nitProb))
}

# Run the viterbi algorithm
# Input: sequence of observations, transmission probability matrix
#        and the probability distribution for the waterholes
# Output: last state of the most likely path for the given observations
runViterbi=function(sequence, transmission, probs){
  
  mostLikelyPosition <- 0
  
  #length of the sequence
  len <- length(sequence)
  
  # The DP matrix has a row for each state and a column for each observation
  dpMatrix <- matrix(0, nrow = 40, ncol = len)
  
  # Only one obvervation
  if (len == 1){
    max <- -Inf
    readings <- sequence[[1]]
    for (i in 1:40){
      prob = calculateEmissionProbability(readings, i, probs)
      if (prob >= max){
        max <- prob
        mostLikelyPosition <- i
      }
    }
  } 
  # Else run full viterbi
  else {
    # Calculate the first state
    readings <- sequence[[1]]
    for (i in 1:40){
      prob = calculateEmissionProbability(readings, i, probs)
      dpMatrix[1,1] <- prob
      # print(prob)
    }
    # Iterate over all observations
    for (i in 2:len){
      readings <- sequence[[i]]
      # Iterate over all states
      for (j in 1:40){
        emProb <- calculateEmissionProbability(readings, j, probs)
        # Find most likely transition from last states
        maxTrans <- -Inf
        for (s in 1:40){
          #probTrans <- transmission[s,j] * dpMatrix[s, i-1]
          probTrans <- log(transmission[s,j]) + dpMatrix[s, i-1]
          if (probTrans >= maxTrans){
            maxTrans <- probTrans
          }
        }
        # Fill dp matrix with maximizing probability
        dpMatrix[j,i] <- (maxTrans + emProb)
      }
    }
  }
  # print(dpMatrix)
  
  # Return the most likely location of croc
  positionLikelihood <- -Inf
  for (i in 1:40){
    if (dpMatrix[i,len] >= positionLikelihood) {
      positionLikelihood <- dpMatrix[i, len]
      mostLikelyPosition <- i
    }
  }
  
  
  return (mostLikelyPosition)
}

testFunction=function(runs){
  res <- 0
  for (i in 1:runs){
    tmp <- runWheresCroc(makeMoves = hmmWC)
    res <- res + tmp
  }
  return (res/runs)
}