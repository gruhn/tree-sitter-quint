
module counters {
  var n: int
 
  action Init = {
    n' = 1
  }
 
  action Even = all {
    n % 2 == 0,
    n' = n / 2,
  }
 
  action ByThree = all {
    n % 3 == 0,
    n' = 2 * n
  }
 
  action Positive = all {
    n > 0,
    n' = n + 1,
  }
 
  action Next = any {
    Even, ByThree, Positive
  }
 
  run run1 = (n' = 1).then(n' = 2).then(n' = 3).then(n' = 6).then(n' = 3)
 
  run run2 = (Init).then(Positive).then(Positive).then(ByThree).then(Even)
 
  run run3 = (Init).then(Next).then(Next).then(Next).then(Next)
}
