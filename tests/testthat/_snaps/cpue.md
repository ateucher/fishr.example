# cpue provides informative message when verbose

    Code
      cpue(c(100, 200), c(10, 20), verbose = TRUE)
    Message
      Processing 2 records using ratio method
    Output
      Survey Result
      Records: 2 
      Gear factor: 1 
      Method: ratio 
      CPUE values: 10 10 

# cpue error message is informative

    Code
      cpue("not a number", 10)
    Condition
      Error:
      ! 'catch' must be numeric, got character.

# cpue produces no warnings with valid input

    Code
      cpue(catch = c(100, 200, 300), effort = c(10, 20))
    Condition
      Warning in `catch / effort`:
      longer object length is not a multiple of shorter object length
    Output
      Survey Result
      Records: 3 
      Gear factor: 1 
      Method: ratio 
      CPUE values: 10 10 30 

# cpue uses verbosity when option set to TRUE

    Code
      cpue(100, 10)
    Message
      Processing 1 records using ratio method
    Output
      Survey Result
      Records: 1 
      Gear factor: 1 
      Method: ratio 
      CPUE values: 10 

# print.cpue_result displays expected output

    Code
      print(result)
    Output
      Survey Result
      Records: 3 
      Gear factor: 1 
      Method: ratio 
      CPUE values: 10 10 20 

