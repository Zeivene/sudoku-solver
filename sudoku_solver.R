#Sudoku

#1.1

# Determine whether the string contains 81 single digits

sudoku1 <- "307008690915073800600910057750840200020300765106507003000069538861700002039082070"

input.puzzle <- function(s){
  a <- strsplit(s, split = "")
  b <- unlist(a)
  s <- as.numeric(b)
  
  if ((anyNA(s))==TRUE|(length(s)!=81))
  {
    print("not a valid puzzle")
  }
  else
  {
    s <- matrix(s,9,9)
    return(s)
  }
}

P <- input.puzzle(sudoku1)

input.puzzle(sudoku1)



#1.2

# Output an image of Sudoku and remove values that contain 0

show.puzzle <- function(M){
  S <- M[9:1,]
  s <- t(S)
  
  image(1:9, 1:9, s, col = "white", xlab = "", ylab = "", xaxt = "n", yaxt = "n")
  
  abline(h=c(3.5,6.5), v=c(3.5,6.5))
  
  for(i in 1:9){
    for(j in 1:9){
      if(s[i,j] > 0){
        text(i, j, s[i, j])
      }
    }
  }
}
show.puzzle(P)



#1.3.1

#Extract the entire row of the specified entry position

row.i.j <- function(s,i,j){
  return(s[i,])
}
row.i.j(input.puzzle(sudoku1),4,7)



#1.3.2

#Extract the entire column of the specified entry position

col.i.j <- function(s,i,j){
  return(s[,j])
}

col.i.j(P,4,7)



#1.3.3

# Extract the entire 3x3 sub-matrix of the specified entry position by using a mathematical formula

subsq.i.j <- function(s,i,j){
  M <- input.puzzle(sudoku1)
  ja = (i-1)%/%3 *3 +1:3
  jb = (j-1)%/%3 *3+1:3
  x = M[ja,jb]
  return(x)
}

subsq.i.j(P,4,7)



#1.4

#Output the possible numbers to fill in the sub-matrix in which the rows and column does not contain

i.j.possibles <- function(a,i,j){
  if (a[i,j] > 0){
    a[i,j]
  }else{
    s <- 1:9
    s1 <- a[i,]
    s2 <- a[,j]
    x <- subsq.i.j(P,i,j)
    s3 <- as.numeric(x)
    s4 <- setdiff(s,s1)
    s5 <- setdiff(s,s3)
    s6 <- setdiff(s,s2)
    s7 <- intersect(s4,s5)
    s8 <- intersect(s6,s7)
    return (s8)
  }
}

i.j.possibles(P,4,7)



#1.5

# Search for any empty cells that only have 1 possible value to fill in, by using the length of the vector

naked.single <- function(a){
  for (i in 1:9){
    for (j in 1:9){
      z <- i.j.possibles(a,i,j)
      if (length(z)==1 && a[i,j]==0){
        a[i,j] <- z
        
      }
    }
  }
  return(a)
}

show.puzzle(naked.single(P))



#1.6.1

# Return the number 1 when the whole 9x9 matrix is filled in, and return 0 when the matrix is incomplete

test.complete <- function(s){
  n <- length(which(s==0))
  if(n==0){
    print("1")
  }else{
    print("0")
  }
}

test.complete(P)



#1.6.2

# Set a limit of iterations of naked.single until the specified iteration has been reached, or if the Sudoku puzzle is completed

rep.naked.single <- function(time){
  while(time!=0){
    a <- naked.single(P)
    a <- naked.single(a)
    a
    time=time-1
  }
  return(a)
}

a <- rep.naked.single(10)
test.complete(a)
show.puzzle(a)



#2.1

# Given its entry position, it calculates all the possible values for other empty cells of its sub-matrix, and if the length of the leftover vector is 1, it outputs the number and the position of the cell that contain a hidden single

sudoku2 <- "900410075470090000000000604800109507000800000690000200207530410010067300000000000"

S = input.puzzle(sudoku2)

hidden.single.check <- function(I,J,n){
  i <- (I-1)*3+1:3
  j <- (J-1)*3+1:3
  a <- i.j.possibles(S,i[3],j[3])
  b <- i.j.possibles(S,i[1],j[1]) 
  c <- i.j.possibles(S,i[2],j[2])
  q <- 0
  if(is.element(n,a)==TRUE){
    q <- q+1
    b <- c(i[3],j[3])
  }
  if(is.element(n,b)==TRUE){
    q <- q+1
    b <- c(i[1],j[1])
  }
  if(is.element(n,c)==TRUE){
    q <- q+1
    b <- c(i[2],j[2])
  }
  if(q==1){
    z <- c(n,b)
    return(z)
  }else(
    return(0)
  )
}

hidden.single.check(2,3,1)
hidden.single.check(2,3,2)



#2.2

# After completing a hidden.single.check in a specific sub-matrix, it repeats the same iteration for all other 9 sub-matrices to fill the cells that qualify for hidden single until the matrix is completed or the repetition limit has been reached

fill.sudoku.using.hidden.single.check <- function(s){
  for(I in 1:3){
    for(J in 1:3){
      for(number in 1:9){
        k<- hidden.single.check(I,J,number)
        if(length(k)!=0){
          S[k[2],k[3]] <- number 
        }
      }
    }
  }
  return(s)
}

S <- fill.sudoku.using.hidden.single.check(S)
show.puzzle(S)



#2.3

# Set a limit of iterations to check for the hidden.single as to prevent the code running repeatedly when there are no hidden single available

test <- function(s){
  n <- length(which(s==0))
  if(n==0){
    print("1")
  }else{
    print("0") }
}  

solver.v2<- function(time){
  while(time!=0){
    a <- naked.single(S)
    a <- fill.sudoku.using.hidden.single.check(a)
    a <- naked.single(a)
    time=time-1
  }
  return(a)
}

a <- solver.v2(10)
test(a)

show.puzzle(a)
