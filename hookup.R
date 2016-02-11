hookup <- function( who, p = n - 1, orderMe = TRUE ) {
#  DESCRIPTION:
#  Randomly assign p partners to each person named in who.
#
#  ARGUMENTS:
#  who  character vector, giving the names of people to be hooked up.
#       Duplicate values are allowed, but probably will cause confusion.
#       Must have length > 1.
#  p    integer vector of length 1, giving the number of partners to assign
#       to each member of who.  Must be between 1 and length(who) - 1.
#       Default is n - 1.
#  orderMe  logical flag.  If TRUE (the default), rows of return matrix
#       will be ordered to align with sort(who).  Otherwise, rows will align
#       with 'who'.
#  VALUE:
#       a matrix of the same mode as 'who', with length(who) rows and p
#       columns.  Row i gives the set of partners for person sort(who)[i].
#  EXAMPLES:
#       hookup(c("B", "A", "D", "C"), 2)
#       hookup(c("B", "A", "D", "C"), 2, orderMe = FALSE)
#       hookup(1:4, 3)
#       
   n <- length(who)
   if ( n < 2 )
      stop( paste( "What, exactly, is it you would like me to do",
         "with input like that?" ) )
   if ( p < 1 || p >= n )
      stop( "Oy.  Why you give me p values like that?" )
   # Construct a non-randomized latin square, excluding column 1.
   # If wanted to include column 1, use:
   #    out <- array(NA, c(n, n) )
   #    out[, 1] <- nseq
   #    ...
   #    out[, i + 1] <- c(nseq[-(seq_len(i))], nseq[seq_len(i)])
   nseq <- seq_len(n)
   out <- array(NA, c(n, n - 1) )
   for(i in nseq[-n])
      out[, i] <- c(nseq[-(seq_len(i))], nseq[seq_len(i)])
   # Pick a random subset of the columns.
   out <- out[ , sample.int(n - 1, size = p), drop = FALSE ]
   # Randomly permute who, and assign to the indices in out.
   rwho <- who[ sample.int(n) ]
   out[] <- rwho[ as.vector(out) ]
   cnames <- paste("Partner", seq_len(p), sep = ".")
   dimnames(out) <- list( as.character(rwho), cnames )
   # Order by who, if desired.  Otherwise, put back in original who order.
   if (orderMe)
      out <- out[ order(rwho), , drop = FALSE ] 
   else
      out <- out[ match( who, rwho ), , drop = FALSE ] 
   out
}
