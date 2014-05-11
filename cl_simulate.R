## Simulating chutes and ladders

straight_board <- rep(0,times=100)

standard_board <- c(
38-1, 0, 0, 14-4, 0,
0, 0, 0, 31-9, 0,
0, 0, 0, 0, 0,
16-6, 0, 0, 0, 0,
42-21, 0, 0, 0, 0,
0, 0, 84-28, 0, 0,
0, 0, 0, 0, 0,
44-36, 0, 0, 0, 0,
0, 0, 0, 0, 0,
0, 0, 26-48, 11-49,0,
67-51,0,0,0,0,
53-56,0,0,0,0,
0,19-62,0,60-64,0,
0,0,0,0,0,
91-71,0,0,0,0,
0,0,0,0,100-80,
0,0,0,0,0,
0,24-87,0,0,0,
0,0,73-93,0,75-95,
0,0,78-98,0,0
)

simulate <- function(board, start=0, simcount=100, turncap=100, diecap=6) {
  outcomes <- vector()
  if (start >0) {
    if ( board[start]!=0) {
      return(NA)
    }
  }
  for (simulation in 1:simcount) {
    location <- start
    
    for (turn in 1:turncap) {
      roll <- sample(1:diecap, 1)
      old_location <- location
      location <- location+roll
      
      
      if (location > length(board)) {
        location <- old_location
      } else {
        location <- (location+board[location])
        
#         print(sprintf("%d -> %d on %d",old_location,location,roll))
      }
      
      if (location == length(board)) {
        outcomes <- c(outcomes, turn)
#         print ("You Win!")
        break
      }
    }
  }
  return(outcomes)
}

# start_outcomes <- rep(0,times=2*length(board))
# dim(start_outcomes) <- c(length(board),2)

# for (start in 1:length(board)) {
#   outcomes <- simulate(board, start, 1000)
#   start_outcomes[start,1] <- mean(outcomes)
#   start_outcomes[start,2] <- sd(outcomes)
# }
# 
# x=1:length(board)
# avg <- start_outcomes[,1]
# sdev <- start_outcomes[,2]
# 
# print(sdev)
# print(avg)
# 
# lim <- print(range(c(avg[!is.na(avg)]-sdev[!is.na(avg)], avg[!is.na(avg)]+sdev[!is.na(avg)])))
# 
# plot(x, avg,
#      ylim=lim,
#      pch=19, xlab="Start Position", ylab="TTC Avg +/- SD",
#      main="Average finish time by board location"
# )
# # hack: we draw arrows but with very special "arrowheads"
# arrows(x, avg-sdev, x, avg+sdev, length=0.05, angle=90, code=3)
