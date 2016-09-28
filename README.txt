WheresCroc Lab Assignment

What has been done so far:

- 	Set up of an HMM model, where the states are the 40 different waterholes, and the observations
	are the readings for salinity, nitrogen and phosphate. Since the observations are continuous distributions
	rather than concrete values, I derived emission probabilities as follows:
	--> for each value (s, n and p), the emission probability for a certain waterhole is
			- 0.68 if it lies within 1 standard deviation of the waterhole distribution
			- 0.27 if it lies within 2 standard deviations of the waterhole distribution
			- 0.01 else
		
	--> the final emission probability of a observation and a waterhole is the product of the emission probabilites of
		all 3 values
	
	To calculate the most probable position of croc, the Viterbi-algorithm was implemented and the last state of the 
	most probable path was assumed to be Crocs position. (No need for traceback). The sequence of observation is saved
	in each round and passed on via the $mem variable to the next round. So the longer the game, the longer the sequence.


-	To find the shortest path to Croc, I fitted our A*-star implementation to this problem. I did not come up with a heuristic,
	so it is basically Dijkstra at the moment. But then again we don't need to be fast in calculating the next step...

-	Each round we can make two moves. Currently, the ranger takes 2 moves in the direction of croc if the crocodile can't be
	reached within one step. Else, the ranger takes one step and then searches the waterhole.

-	The fact that tourists can be eaten and thereby show the position of croc is not yet considered and still needs implementing
EDIT: Has now been implemented. When tourists are eaten, the probability for this state in the sequence is set to 1.

Note: The estimation of Crocs position is sometimes really good, then Croc is found in a few steps. However sometimes it is really bad and
then it can take a long time ... I'm not sure if it is because of the emission probability model I used or because the readings are just so
all over the place (which they are sometimes). But we might wanna try something else out there.	