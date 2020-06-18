body <- setRefClass('body',
                    fields = c(
                      'mass',
                      'position',
                      'velocity',
                      'acceleration'
                    ),
                    methods = list(
                      initialize = function(m, p, v, a){
                        setMass(m)
                        setPosition(p)
                        setVelocity(v)
                        setAcceleration(a)
                      },
                      setMass = function(m){
                        validateMass(m)
                        mass <<- m
                      },
                      setPosition = function(p){
                        validateVector(p)
                        position <<- p
                      },
                      setVelocity = function(v){
                        validateVector(v)
                        velocity <<- vect(v)
                      },
                      setAcceleration = function(a){
                        validateVector(a)
                        acceleration <<- vect(a)
                      },
                      validateMass = function(m){
                        if(!is.numeric(m)){
                          stop("Mass is not numeric")
                        }
                        if(any(is.nan(m))){
                          stop("Mass is NaN")
                        }
                        if(is.infinite(m)){
                          stop("Mass cannot be infinite")
                        }
                        if(length(m) > 1){
                          stop("Mass must be a single value")
                        }
                      },
                      validateVector = function(v){
                        if(!is.numeric(v)){
                          stop("Input is not numeric")
                        }
                        if(any(is.nan(v))){
                          stop("Input contains NaN")
                        }
                        if(any(is.infinite(v))){
                          stop("Input cannot be infinite")
                        }
                      }
                    ))