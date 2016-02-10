hookup <- function( who, p = n - 1 ) {
   n <- length(who)
   if ( p >= n )
      stop( "p >= n, that is too weird.")
   whonum <- seq_len( n )
   nummat <- array( NA, c(n, p) )
   counts <- numeric( n )
   for ( i in whonum ) {
      picks <- sample( whonum[ whonum != i & counts < p], p )
      print(picks)
      counts[picks] <- counts[picks] + 1
      print(counts)
      nummat[i, ] <- picks
   }
   out <- who[ as.vector(nummat) ]
   dim(out) <- c(n, p)
   out
}

# Oops!  This not working quite right.  SOMETIMES, it chooses some people
# > p times and others < p times.  I do not currently see how that is possible!

# Also, add who as rownames to out.

# OK.  Fix this by using sample.int instead of sample.  See Examples under
# ?sample for an example!!

# Also, need to modify algorithm to avoid the (somewhat rare) case that I
# run out of candidates by the last person.  I have a fix for that...
