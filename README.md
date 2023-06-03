cabal build 

cabal exec TicTacToe

To pass command line arguments
cabal exec TicTacToe -- -s 15 -p X

Player ->
let p1 :: Player
    p1 = X

let p2 :: Player
    p2 = O