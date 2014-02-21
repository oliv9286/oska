-- Keefe Law 43674100 i7w7
-- Olivia (Ningyuan) Zhang 44406106 m4s7

{- ******************************
         Variables
    ***************************** -}

test_board = ["----", "-ww", "b-", "--b", "----"]
empty_space = '-'
    
    
{- ******************************
        Primary function call
    ***************************** -}
    
-- Checks if the board is in its goal state; otherwise look ahead
--   the specified number of steps to look for best move.      
oska_i7w7 :: [[Char]] -> Char ->Int-> [[Char]]
oska_i7w7 board player num_moves
    | is_gameover_i7w7 (check_gameOver_i7w7 board player opponent) = board
    | otherwise                    = next_move_i7w7 num_moves player board
    where opponent = get_opponent_i7w7 player




-- Gets the next best moved based on a minimax algorithm.
next_move_i7w7 :: Int -> Char -> [[Char]] -> [[Char]]
next_move_i7w7 depth_limit player board
    = (!!) next_states max_val_index
    where next_states      = generate_states_i7w7 board player player_positions
          player_positions = get_player_pieces_positions_i7w7 board player 
          opponent         = get_opponent_i7w7 player
          minimax_i7w7_result   = map (minimax_i7w7 0 depth_limit opponent minimum) next_states
          maximum_value    = maximum minimax_i7w7_result
          max_val_index    = get_index_i7w7 minimax_i7w7_result maximum_value

-- Return the index given a list and an element.
get_index_i7w7 :: [Int] ->Int -> Int
get_index_i7w7 list value
    | (head list) == value = 0
    | otherwise = 1 + get_index_i7w7 (tail list) value



{- ******************************
        Minimax algorithm
    ***************************** -}

-- Gets either the minium or maximum board value.
minimax_i7w7 :: Int -> Int -> Char -> ([Int] -> Int) -> [[Char]] -> Int
minimax_i7w7 depth depth_limit player eval_function board
    | gameover        = evaluate_board_i7w7 board player
    | otherwise    = eval_function children
    where opponent    = get_opponent_i7w7 player
          next_states  = generate_states_i7w7 board player player_positions
          children    = map (minimax_i7w7 (depth+1) depth_limit opponent eval_function) next_states
          gameover       = (depth == depth_limit) || (is_gameover_i7w7 (check_gameOver_i7w7 board player (get_opponent_i7w7 player))) || null children
          player_positions = (get_player_pieces_positions_i7w7 board player)

-- Evaluates the minimax score for the given board.
-- Simply looks at number of pieces belonging to each player.
evaluate_board_i7w7 :: [[Char]] -> Char -> Int
evaluate_board_i7w7 board player
    | check_gameOver_i7w7 board player (get_opponent_i7w7 player) == player = 10
    | check_gameOver_i7w7 board player (get_opponent_i7w7 player) == get_opponent_i7w7 player = -10
    | check_gameOver_i7w7 board player (get_opponent_i7w7 player) == 'd' = 0
    | otherwise = num_player - num_opponent
        where num_player = (count_all_pieces_i7w7 board player) 
              num_opponent = (count_all_pieces_i7w7 board (get_opponent_i7w7 player))

-- If the given function is maximum, return minimum, and vice versa.
alternate_evaluator_i7w7 :: ([Int]->Int) -> ([Int]->Int)
alternate_evaluator_i7w7 evaluator
    | evaluator [1,2]  == 1 = maximum
    | otherwise = minimum



{- ******************************
        Goal-state checks
    ***************************** -}
    
-- Check if player is on the opposite end of the board (winning state).
on_opponent_side_i7w7 :: [[Char]] -> Char -> Bool
on_opponent_side_i7w7 board player
    | player == 'w' = all_pieces_at_bottom_i7w7 board 'w'
    | otherwise = all_pieces_at_bottom_i7w7 (reverse board) 'b'

-- Check if all of the player's pieces are at the end of the board.
all_pieces_at_bottom_i7w7 :: [[Char]] -> Char ->Bool
all_pieces_at_bottom_i7w7 board player =
        count_all_pieces_i7w7 board player /= 0 && (count_all_pieces_i7w7 board player) == (count_pieces_in_row_i7w7 (last board) player)

-- Check if player has lost all pieces.
removed_all_opponents_i7w7 :: [[Char]]-> Char -> Bool
removed_all_opponents_i7w7 board player
    | player == 'w' = (count_all_pieces_i7w7 board 'b') == 0
    | otherwise = (count_all_pieces_i7w7 board 'w') == 0

-- For the given board, check if game is over, return the winning player, 
--   otherwise, return empty space if game is not over.
check_gameOver_i7w7 :: [[Char]] -> Char->Char -> Char
check_gameOver_i7w7 board player opponent
    | (count_all_pieces_i7w7 board player) ==0 = opponent
    | (count_all_pieces_i7w7 board opponent) == 0 = player
    | (on_opponent_side_i7w7 board player) && (on_opponent_side_i7w7 board opponent) && ((count_all_pieces_i7w7 board player) > (count_all_pieces_i7w7 board opponent)) = player
    | (on_opponent_side_i7w7 board player) && (on_opponent_side_i7w7 board opponent) && ((count_all_pieces_i7w7 board player) < (count_all_pieces_i7w7 board opponent)) = opponent
    | (on_opponent_side_i7w7 board player) && (on_opponent_side_i7w7 board opponent) && ((count_all_pieces_i7w7 board player) == (count_all_pieces_i7w7 board opponent)) = 'd'
    | on_opponent_side_i7w7 board player = player
    | on_opponent_side_i7w7 board opponent = opponent
    | otherwise = '-'

-- Checks if the game is draw.
check_draw_i7w7 :: Char -> Bool 
check_draw_i7w7 status
    | status == 'd' = True
    | otherwise = False

-- Checks if game is over.
is_gameover_i7w7 :: Char -> Bool
is_gameover_i7w7 status
    | (status == 'w') || (status == 'b') || (status == 'd') = True
    | otherwise = False



{- ******************************
        Move generation
    ***************************** -}
    
-- Generates all possible next states for a player with a the given board.
generate_states_i7w7 :: [[Char]] -> Char-> [(Int,Int)]-> [[[Char]]]
generate_states_i7w7 board player positions 
    | null positions = []
    | otherwise = (generate_moves_i7w7 board player (snd(head positions)) (fst(head positions))) ++ (generate_states_i7w7 board player (tail positions))

-- Generate a list of possible moves for a given piece.
generate_moves_i7w7 :: [[Char]]->Char->Int->Int->[[[Char]]]
generate_moves_i7w7 board player index row_num
    | player == 'w'         = moves_down_i7w7 board index row_num ++ jumps_down_i7w7 board index row_num
    | otherwise                     = moves_up_i7w7 board index row_num ++ jumps_up_i7w7 board index row_num

-- Check and generate possible upward slides for a given piece.
moves_up_i7w7 :: [[Char]]->Int->Int->[[[Char]]]
moves_up_i7w7 board index row_num =
        add_states_i7w7 mu_left mu_right
        where mu_left
                | is_mu_left_possible_i7w7 board index row_num       = move_up_left_i7w7 board 'b' index row_num
                | otherwise                                                                     = []
              mu_right
                | is_mu_right_possible_i7w7 board index row_num      = move_up_right_i7w7 board 'b' index row_num
                | otherwise                                                                     = []
                  
-- check and generate downward movements if possible for a given piece
moves_down_i7w7 :: [[Char]]->Int->Int->[[[Char]]]
moves_down_i7w7 board index row_num =
        add_states_i7w7 md_left md_right
        where md_left
                | is_md_left_possible_i7w7 board index row_num        = move_down_left_i7w7 board 'w' index row_num
                | otherwise    = []
              md_right
                | is_md_right_possible_i7w7 board index row_num      = move_down_right_i7w7 board 'w' index row_num
                | otherwise    = []

 -- check and generate possible upward slides for a given piece
jumps_up_i7w7 :: [[Char]]->Int->Int->[[[Char]]]
jumps_up_i7w7 board index row_num =
        add_states_i7w7 ju_left ju_right
        where ju_left
                | is_ju_left_possible_i7w7 board 'b' index row_num       = jump_up_left_i7w7 board 'b' index row_num
                | otherwise                                                                     = []
              ju_right
                | is_ju_right_possible_i7w7 board 'b' index row_num      = jump_up_right_i7w7 board 'b' index row_num
                | otherwise                                                                     = []

-- check and generate downward jumps if possible for a given piece
jumps_down_i7w7 :: [[Char]]->Int->Int->[[[Char]]]
jumps_down_i7w7 board index row_num =
        add_states_i7w7 jd_left jd_right
        where jd_left
                | is_jd_left_possible_i7w7 board 'w' index row_num           = jump_down_left_i7w7 board 'w' index row_num
                | otherwise     = []
              jd_right
                | is_jd_right_possible_i7w7 board 'w' index row_num          = jump_down_right_i7w7 board 'w' index row_num
                | otherwise     = []

-- adds two states 
add_states_i7w7 :: [[Char]] -> [[Char]] -> [[[Char]]]
add_states_i7w7 state1 state2
    | null state1 && null state2 = []
    | null state1                = [state2]
    | null state2                = [state1]
    | otherwise                  = [state1, state2]
    


{- ******************************
        Move/jump checks
    ***************************** -}

--checks if moving up left is possible for the piece
is_mu_left_possible_i7w7 :: [[Char]]->Int->Int->Bool
is_mu_left_possible_i7w7 board index row_num
    | row_num == 0 = False
    | (row_num > ((length board)-1)`div` 2) && index == 0 =  False
    | (row_num > ((length board)-1)`div` 2) && index /=0 = (board!!(row_num-1))!!(index-1) == empty_space
    | otherwise = (board!!(row_num-1))!!index == empty_space

--checks if moving up right is possible for the piece
is_mu_right_possible_i7w7 :: [[Char]]->Int->Int->Bool
is_mu_right_possible_i7w7 board index row_num
    | row_num == 0 = False
    | (row_num > ((length board)-1)`div` 2) && (index == (length (board!!row_num))-1) = False
    | (row_num > ((length board)-1)`div` 2) = (board!!(row_num-1)!!index) ==  empty_space
    | otherwise = (board!!(row_num-1))!!(index+1) == empty_space

--checks if moving down left is possible for the piece
is_md_left_possible_i7w7 :: [[Char]]->Int->Int->Bool
is_md_left_possible_i7w7 board index row_num
    | row_num == (length board)-1           = False
    | row_num < ((length board)-1)`div`2 && index == 0     = False
    | row_num < ((length board)-1)`div`2   = (board!! (row_num +1))!! (index -1) == '-'
    | otherwise                                                     = (board!! (row_num +1))!! index        == '-'

--checks if moving down right is possible for the piece
is_md_right_possible_i7w7 :: [[Char]]->Int->Int->Bool
is_md_right_possible_i7w7 board index row_num
    | row_num == (length board)-1                   = False
    | row_num <= ((length board)-1)`div`2 && (index == (length (board!!row_num))-1) = False
    | row_num <= ((length board)-1)`div`2   = (board!! (row_num +1))!! index == '-'
    | otherwise = (board!! (row_num +1))!! (index +1) == '-'


--checks if jumping up left is possible for the piece
is_ju_left_possible_i7w7 :: [[Char]]->Char->Int->Int->Bool
is_ju_left_possible_i7w7 board piece index row_num
    | row_num == 0 || row_num == 1 = False
    | row_num > ((length board)-1)`div`2 + 1 && (index == 0 || index ==1) = False
    | row_num == (((length board)-1)`div`2) + 1 && (index == 0) =  False
    | row_num == (((length board)-1)`div`2) + 1 && (index /=0) = (board!!(row_num-1)!!(index-1) == get_opponent_i7w7(piece)) && (board!!(row_num-2)!!(index-1) == empty_space)
    | row_num > ((length board)-1)`div`2 + 1  = (board!!(row_num-1)!!(index-1) == get_opponent_i7w7(piece)) && (board!!(row_num-2)!!(index-2)) == empty_space
    | otherwise = (board!!(row_num-1)!!index == get_opponent_i7w7(piece)) && (board!!(row_num-2)!!index) == empty_space

--checks if jumping up right is possible for the piece
is_ju_right_possible_i7w7 :: [[Char]]->Char->Int->Int->Bool
is_ju_right_possible_i7w7 board piece index row_num
    | row_num == 0 || row_num == 1 = False
    | row_num > ((length board)-1)`div`2 + 1 &&  (index == (length (board!!(row_num-1))) || index == (length (board!!(row_num-2))))= False
    | row_num == (((length board)-1)`div`2) + 1 && (index == (length (board!!(row_num-1)))) = False
    | row_num == (((length board)-1)`div`2) + 1 = (board!!(row_num-1)!!index) ==  get_opponent_i7w7(piece) && (board!!(row_num-2)!!(index+1) ==  empty_space)
    | row_num > ((length board)-1)`div`2 + 1 = (board!!(row_num-1)!!index) == get_opponent_i7w7(piece) && (board!!(row_num-2)!!index) == empty_space
    | otherwise = (board!!(row_num-1)!!(index+1) == get_opponent_i7w7(piece)) && (board!!(row_num-2)!!(index+2) == empty_space)

--checks if jumping down left is possible for the piece
is_jd_left_possible_i7w7 :: [[Char]]->Char->Int->Int->Bool
is_jd_left_possible_i7w7 board piece index row_num
    | row_num >= (length board)-2           = False
    | row_num == (((length board)-1)`div`2) -1 && index == 0        = False
    | row_num < (((length board) -1)`div`2) -1 && (index == 0 || index == 1)        = False
    | row_num < (((length board) -1)`div`2) -1              = (board!! (row_num +1))!! (index -1) == get_opponent_i7w7 piece && (board!! (row_num +2))!! (index -2) == empty_space
    | row_num == (((length board) -1)`div`2) -1             = (board!! (row_num +1))!! (index -1) == get_opponent_i7w7 piece && (board!! (row_num +2))!! (index -1) == empty_space
    | otherwise                                                                                                                                     = (board!! (row_num +1))!! index == get_opponent_i7w7 piece && (board!! (row_num +2))!! index == empty_space

--checks if jumping down right is possible for the piece
is_jd_right_possible_i7w7 :: [[Char]]->Char->Int->Int->Bool
is_jd_right_possible_i7w7 board piece index row_num
    | row_num >= (length board)-2           = False
    | row_num == (((length board)-1)`div`2) -1 && index == (length (board!!row_num)) -1             = False
    | row_num < (((length board)-1)`div`2) -1 && (index == (length (board!!row_num)) -1 || index == (length (board!!row_num)) -2)   = False
    | row_num < (((length board) -1)`div`2) -1                                                                      = (board!! (row_num +1))!! index == get_opponent_i7w7 piece && (board!! (row_num +2))!! index == empty_space
    | row_num == (((length board) -1)`div`2) -1                                                                     = (board!! (row_num +1))!! index == get_opponent_i7w7 piece && (board!! (row_num +2))!! (index +1) == empty_space
    | otherwise                                                                                                                                     = (board!! (row_num +1))!! (index +1) == get_opponent_i7w7 piece && (board!! (row_num +2))!! (index +2) == empty_space



{- ******************************
        Move / jump functions
    *****************************-}

-- moves the piece diagonally upwards to the left (up 1, left 1).
move_up_left_i7w7 :: [[Char]]->Char->Int->Int->[[Char]]
move_up_left_i7w7 board player index row_num 
    | row_num <= ((length board)-1)`div`2 = replace_row_i7w7 index (row_num -1) player (replace_row_i7w7 index row_num empty_space board)
    | otherwise = replace_row_i7w7 (index-1) (row_num-1) player (replace_row_i7w7 index row_num empty_space board)

-- moves the piece diagonally upwards to the right (up 1, right 1).
move_up_right_i7w7 :: [[Char]]->Char->Int->Int->[[Char]]
move_up_right_i7w7 board player index row_num
    | row_num <= ((length board)-1)`div`2 = replace_row_i7w7 (index+1) (row_num-1) player (replace_row_i7w7 index row_num empty_space board)
    | otherwise = replace_row_i7w7 index (row_num-1) player (replace_row_i7w7 index row_num empty_space board)

-- moves the piece diagonally downwards to the left (down 1, left 1).
move_down_left_i7w7 :: [[Char]]->Char->Int->Int->[[Char]]
move_down_left_i7w7 board player index row_num
    | row_num < (length board)`div`2    = replace_row_i7w7 (index -1) (row_num +1) player (replace_row_i7w7 index row_num empty_space board)
    | otherwise                         = replace_row_i7w7 index (row_num +1) player (replace_row_i7w7 index row_num empty_space board)
    
-- moves the piece diagonally downwards to the right (down 1, right 1).
move_down_right_i7w7 ::[[Char]]->Char->Int->Int->[[Char]]
move_down_right_i7w7 board player index row_num
    | row_num < ((length board)-1)`div`2   = replace_row_i7w7 index (row_num +1) player (replace_row_i7w7 index row_num empty_space board)
    | otherwise                         = replace_row_i7w7 (index +1) (row_num +1) player (replace_row_i7w7 index row_num empty_space board)

-- moves the piece diagonally upwards to the left by 2 spaces (up 2, left 2), removing the piece in between.
jump_up_left_i7w7::[[Char]]-> Char -> Int -> Int -> [[Char]]
jump_up_left_i7w7 board player index row_num
    | row_num <= ((length board)-1)`div`2 = (replace_row_i7w7 index (row_num-2) player (replace_row_i7w7 index (row_num-1) empty_space (replace_row_i7w7 index row_num empty_space board)))
    | row_num == (((length board)-1)`div`2) + 1 =  (replace_row_i7w7 (index-2) (row_num-2) player (replace_row_i7w7 (index-1) (row_num-1) empty_space (replace_row_i7w7 index row_num empty_space board)))
    | otherwise = (replace_row_i7w7 (index-2) (row_num-2) player (replace_row_i7w7 (index-1) (row_num-1) empty_space (replace_row_i7w7 index row_num empty_space board)))

-- moves the piece diagonally upwards to the right by 2 spaces (up 2, right 2), removing the piece in between.
jump_up_right_i7w7::[[Char]] -> Char -> Int -> Int-> [[Char]]
jump_up_right_i7w7 board player index row_num
    | row_num <= ((length board)-1)`div`2 = replace_row_i7w7 (index+2) (row_num-2) player (replace_row_i7w7 (index+1) (row_num-1) empty_space (replace_row_i7w7 index row_num empty_space board))
    | row_num == (((length board)-1)`div`2) + 1 = replace_row_i7w7 (index+1) (row_num-2) player (replace_row_i7w7 (index+1) (row_num-2) empty_space (replace_row_i7w7 index row_num empty_space board))
    | otherwise = replace_row_i7w7 index (row_num -2) player (replace_row_i7w7 index (row_num -1) empty_space (replace_row_i7w7 index row_num empty_space board))

-- moves the piece diagonally downwards to the left by 2 spaces (down 2, left 2), removing the piece in between.
jump_down_left_i7w7::[[Char]] -> Char -> Int-> Int -> [[Char]]
jump_down_left_i7w7 board player index row_num
    | row_num < (((length board)-1)`div`2)-1        = replace_row_i7w7 (index -2) (row_num +2) player (replace_row_i7w7 (index -1) (row_num +1) empty_space (replace_row_i7w7 index row_num empty_space board))
    | row_num == (((length board)-1)`div`2)-1       = replace_row_i7w7 (index -1) (row_num +2) player (replace_row_i7w7 (index -1) (row_num +1) empty_space (replace_row_i7w7 index row_num empty_space board))
    | otherwise                                                             = replace_row_i7w7 index (row_num +2) player (replace_row_i7w7 index (row_num +1) empty_space (replace_row_i7w7 index row_num empty_space board))

-- moves the piece diagonally downwards to the right by 2 spaces (down 2, right 2), removing the piece in between.
jump_down_right_i7w7:: [[Char]] -> Char -> Int -> Int-> [[Char]]
jump_down_right_i7w7 board player index row_num
    | row_num < (((length board)-1)`div`2)-1    = replace_row_i7w7 index (row_num +2) player (replace_row_i7w7 index (row_num +1) empty_space (replace_row_i7w7 index row_num empty_space board))
    | row_num == (((length board)-1)`div`2)-1       = replace_row_i7w7 (index +1) (row_num +2) player (replace_row_i7w7 index (row_num +1) empty_space (replace_row_i7w7 index row_num empty_space board))
    | otherwise                                                                     = replace_row_i7w7 (index +2) (row_num +2) player (replace_row_i7w7 (index +1) (row_num +1) empty_space (replace_row_i7w7 index row_num empty_space board))



{- ******************************
        Row replacement
    *****************************-}
    
replace_row_i7w7 :: Int -> Int -> Char -> [[Char]]-> [[Char]]
replace_row_i7w7 index row_num value board
    | null board    = []
    | row_num == 0  = (replace_value_i7w7 value index (board!!row_num)) : (tail board)
    | otherwise     = (head board): (replace_row_i7w7 index (row_num-1) value (tail board))

replace_value_i7w7 :: Char -> Int -> [Char] -> [Char]
replace_value_i7w7 value x row 
    | null row      = []
    | x == 0        = value : replace_value_i7w7 value (x - 1) (tail row)
    | otherwise     = (head row) : replace_value_i7w7 value (x - 1) (tail row)
        
        
        
{- ******************************
        Miscellaneous helper functions
    ***************************** -}
        
-- Counts all of a player's pieces.
count_all_pieces_i7w7 :: [[Char]] -> Char ->Int 
count_all_pieces_i7w7 board player
    | null board = 0 
    | otherwise = (count_pieces_in_row_i7w7 (head board) player) + (count_all_pieces_i7w7 (tail board) player)

-- Counts a player's pieces in a given row.
count_pieces_in_row_i7w7 :: [Char] -> Char ->Int
count_pieces_in_row_i7w7 row player
    | null row = 0 
    | (head row) == player = 1 + (count_pieces_in_row_i7w7 (tail row) player)
    | otherwise = (count_pieces_in_row_i7w7 (tail row) player)
        
-- Return the player's opponent.
get_opponent_i7w7 :: Char -> Char
get_opponent_i7w7 player
    | player == 'w' = 'b'
    | player == 'b' = 'w'
    | otherwise     = 'n'

-- Takes a board and gets a list of positions for all pieces of the given player.
get_player_pieces_positions_i7w7 :: [[Char]]->Char->[(Int,Int)]
get_player_pieces_positions_i7w7 board player = get_player_pieces_positions_helper_i7w7 board player 0

get_player_pieces_positions_helper_i7w7 :: [[Char]]->Char ->Int-> [(Int,Int)]
get_player_pieces_positions_helper_i7w7 board player acc
    | null board = []
    | otherwise = (get_player_pos_row_i7w7 (head board) acc player) ++  (get_player_pieces_positions_helper_i7w7 (tail board) player (acc+1))
    
-- Generate a list of player pieces' indexes in a given row, with a given row number.
get_player_pos_row_i7w7 :: [Char]->Int->Char->[(Int, Int)]
get_player_pos_row_i7w7 row row_num player 
    | null row = []
    | otherwise = get_player_pos_row_helper_i7w7 row row_num player 0

get_player_pos_row_helper_i7w7 :: [Char] -> Int -> Char ->Int-> [(Int,Int)]
get_player_pos_row_helper_i7w7 row row_num player acc
    | null row = []
    | player == (head row) = (row_num, acc): (get_player_pos_row_helper_i7w7 (tail row) row_num player (acc + 1))
    | otherwise = get_player_pos_row_helper_i7w7 (tail row) row_num player (acc + 1)

-- Count the number of a player's current pieces on the board.
count_numPieces_board_i7w7 :: [[Char]]->Char->Int
count_numPieces_board_i7w7 board player
    | null board = 0
    | otherwise = count_numPieces_line_i7w7 (head board) player + (count_numPieces_board_i7w7 (tail board) player)

-- Count the number of a player's current pieces in a particular row.
count_numPieces_line_i7w7 :: [Char]->Char->Int
count_numPieces_line_i7w7 row player
    | null row = 0
    | player == (head row) = 1 + count_numPieces_line_i7w7 (tail row) player
    | otherwise = count_numPieces_line_i7w7 (tail row) player