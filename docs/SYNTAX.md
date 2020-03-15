# Syntax

## Point scores

* All points scores as specified as floating point numbers
* Atoms must have integral values. 
  
For example
```
\begin{problem}[17.]
Give a closed-form
solution in terms of $\Theta$ for the following recurrences.  Also, state
whether the recurrence is dominated at the root, the leaves, or
equally at all levels of the recurrence tree.

\ask[9.]  ---> This primary prompt distributes 9 points over 1.5 factors
Give closed form for  
$f(n) = 5f(n/5) + \Theta(n)$

\sol[0.5]  --> So this prompt gets 3 points
$\Theta (n \lg n)$, balanced.

\sol[1.0]  ---> This one gets 6 points.
$\Theta (n \lg n)$, balanced.

\onechoice[8]  --> This distributes 8 points over 2 factors 

\choice* Balanced  --> This gets 4 points (its factor is 1 by default 
\choice[0.4] Leaves Dominated  --> This gets 3.2 points
\choice[0.6] Root dominated       --> This one gets 4.8 points
``` 

