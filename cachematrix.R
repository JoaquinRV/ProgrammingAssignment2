## There are two R files for Programming Assignment2 and are as follows:
## 1.  makeCacheMatrix.R -- set various functions whose primary input is
##                          a valid matrix.  These functions are:
##      a.  ver_mat -- validate a matrix, if it is numeric, and if it is a square matrix
##      b.  get_mat -- retrieve the matrix as initially passed as an argument of makeCacheMatrix function
##      c.  set_mat -- once the matrix is validated, it is stored in an environment for permanency
##      d.  get_inv -- retrieve the inverse matrix from cache storage
##      e.  set_inv -- the calculated inverse matrix is stored in an environment for permanency
## 2.  cacheSolve.R -- the purpose of this function is to calculate the inverse matrix or 
##                     if it was previously calculated, it retrieves the inverse matrix from cache.

## makeCacheMatrix.R
## Purpose:  to define various functions as described above for a valid matrix.
##           (see above description of the methods created related to the matrix passed
##            as an argument to makeCacheMatrix.R)


makeCacheMatrix = function(mat = matrix())
   {
      cached_inv = NULL
      ver_mat = function(mat)
                {
                   flg = NULL
                   if(!is.matrix(mat))
                      { message("Msg#1:  not a matrix")
                        flg = 1
                      }
                   if(!is.numeric(mat))
                      { message("Msg#2:  not numeric")
                        flg = 2
                      }
                   if(is.null(flg))
                      {
                        if(dim(mat)[1] != dim(mat)[2])
                          {
                            message("Msg#3: not a square matrix")
                            flg = 3
                          }
                      }
                   flg
                }
      get_mat = function() mat
      set_mat = function(mat)
                {
                   cached_mat <<- mat
                   cached_inv <<- NULL
                }
      get_inv = function() cached_inv
      set_inv = function(inv)
                {
                   cached_inv <<- inv
                }
      list(ver_mat = ver_mat,
           get_mat = get_mat,
           set_mat = set_mat,
           get_inv = get_inv,
           set_inv = set_inv)
   }


## cacheSolve.R
## Purpose:  this function simply calculates the inverse matrix or if it is already
##           in cache storage, the inverse matrix is retrieved.


cacheSolve = function(x, ...)
             {
                data_mat = x$get_mat()
                err = x$ver_mat(data_mat)
                if(!is.null(err))
                   { message("Correct the error")
                     return()
                   }
                message("Matrix is verified correct")

                data_inv = x$get_inv()
                if(!is.null(data_inv))
                   { message("inverse retrieved")
                     return(data_inv)
                   }
                x$set_mat(data_mat)
                data_inv = solve(data_mat)
                message("inverse calculated")
                x$set_inv(data_inv)
                return(data_inv)
             }
