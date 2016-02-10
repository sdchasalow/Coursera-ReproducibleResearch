hookup <- function( who, p = n - 1 ) {
   n <- length(who)
   if ( p >= n )
      stop( "p >= n, that is too weird.")
   whonum <- seq_len( n )
   nmat <- array( NA, c(n, p) )
   counts <- numeric( n )
   # Assign first partner to each person in who:
   # See Examples under ?sample to understand why I use sample.int construct.
   j <- 1
   for ( i in whonum ) {
      cands <- whonum[ whonum != i & counts < j ]
      pick <- cands[ sample.int( length(cands), size = 1 ) ]
      # pick <- sample( cands, 1 ) # This often breaks when length(cands)==1.
      print(pick)
      counts[pick] <- counts[pick] + 1
      print(counts)
      nmat[i, j] <- pick
   }
   # Assign more partners if desired:
   # No need to check p > 1; the for statement as written will handle that.
   for ( j in seq_len(p)[-1] ) {
      for ( i in whonum ) {
         cands <- whonum[ whonum != i & counts < j & whonum != nmat[i, j - 1] ]
         cands <- whonum[ whonum != i & counts < j & !is.element(
            whonum, nmat[i, -j] ) ]
         pick <- cands[ sample.int( length(cands), size = 1 ) ]
         print(pick)
         counts[pick] <- counts[pick] + 1
         print(counts)
         nmat[i, j] <- pick
      }
   }
   out <- who[ as.vector(nmat) ]
   dim(out) <- c(n, p)
   out
}

# Also, add who as rownames to out.

# OK.  Now this gave an error :-(
# hookup(LETTERS[1:4], 2)
# [1] 4
# [1] 0 0 0 1
# [1] 1
# [1] 1 0 0 1
# [1] 2
# [1] 1 1 0 1
# [1] 3
# [1] 1 1 1 1
# [1] 2
# # [1] 1 2 1 1
# [1] 4
# [1] 1 2 1 2
# [1] 1
# [1] 2 2 1 2
# Error in sample.int(length(cands), size = 1) : invalid first argument

# MAYBE I should not check counts < j ??? Just check each time that counts < p?

