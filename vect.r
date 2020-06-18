vect = setRefClass("vect", 
                    fields = list(
                      comp = 'numeric',
                      len = 'numeric',
                      unit = 'numeric'
                    ),
                    methods = list(
                    initialize = function(a, b = NA){
                      validateInput(a)
                      if(length(b) == 1 & anyNA(b)){
                        comp <<- a
                      }
                      else{
                        validateInput(b)
                        validateLength(a, b)
                        
                        comp <<- b - a
                      }
                      len <<- vectLength(comp)
                      unit <<- vectUnit(comp, len)
                    },
                    vectInverse = function(){
                      return(vect(-comp))
                    },
                    vectResize = function(a){
                      comp <<- unit * a
                      len <<- a
                    },
                    vectUnit = function(a, l){
                      return(a/l)
                    },
                    vectLength = function(v){
                      return(sqrt(sum(v^2)))
                    },
                    validateInput = function(a){
                      if(anyNA(a)){
                        stop("Input contains NA values")
                      }
                      if(any(is.nan(a))){
                        stop("Input contains NaN")
                      }
                      if(any(is.infinite(a))){
                        stop("Input contains Inf values")
                      }
                      if(!is.numeric(a)){
                        stop("Input is not numeric")
                      }
                    },
                    validateLength = function(a, b){
                      if(length(a) != length(b)){
                        stop("Input points don't have the same length")
                      }
                    }
                  ))

'%v+%' = function(a, b){
  return(vect(a$comp + b$comp))
}
'%v-%' = function(a, b){
  return(vect(a$comp - b$comp))
}
'%v*%' = function(a, b){
  return(vect(a$comp * b))
}
'%v/%' = function(a, b){
  return(vect(a$comp / b))
}
