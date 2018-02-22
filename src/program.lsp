;; ************************************************************
;; * Name:  Philip Glazman                                    *
;; * Project:  Kono - Lisp Implementation                     *
;; * Class:  CMPS 366 Organization of Programming Languages   *
;; * Date:  2/6/2018                                          *
;; ************************************************************

;; TO RUN: sbcl --non-interactive --load program.lsp

;; Seed randomness. Only use of a global var. Find better solution later. 
;; Solution found on internet: https://stackoverflow.com/questions/4034042/random-in-common-lisp-not-so-random
(setf *random-state* (make-random-state t))

;; /* ********************************************************************* 
;; Function Name: randomDice 
;; Purpose: Returns a number between 2 and 12. Simluates dice roll.
;; Parameters: 
;;             none.
;; Return Value: A number between 2 and 12.
;; Local Variables: 
;;             none.
;; Algorithm: 
;;             1) Use random function to generate random number.
;; Assistance Received: none 
;; ********************************************************************* */
(defun randomDice ()
	(+ 2 (random 11)))

;; /* ********************************************************************* 
;; Function Name: choosefirstPlayer 
;; Purpose: Returns the first player in a list. Will either be (HUMAN) or (COMPUTER).
;; Parameters: 
;;             none.
;; Return Value: List containing (HUMAN) or list containing (COMPUTER). List will contain first player to play game.
;; Local Variables: 
;;             humanDice, holds atom from randomDice function. Represents dice roll for human.
;;             computerDice, holds atom from randomDice function. Represents dice roll for computer.
;; Algorithm: 
;;             1) Get random dice roll for human.
;;			   2) Get random dice roll for computer.
;;			   3) Output dice rolls to human.
;;			   4) If human dice roll is greater than computer dice roll, then return list (HUMAN).
;;		 	   5) If human dice roll is less than computer dice roll, then return list (COMPUTER).
;;			   6) Else if the dice rolls are equal, then recursive call function. 
;; Assistance Received: none 
;; ********************************************************************* */
(defun choosefirstPlayer()
	(let* ((humanDice (randomDice))
			(computerDice (randomDice)))
			(format t "Human rolls ~D. ~%" humanDice)
			(format t "Computer rolls ~D. ~%" computerDice)
			(cond 	((> humanDice ComputerDice)
					(list 'human))
					((< humanDice ComputerDice)
					(list 'computer))
					((= humanDice ComputerDice)
					(choosefirstPlayer)))))

;; /* ********************************************************************* 
;; Function Name: computerColor 
;; Purpose: Returns list holding computer player's color. 
;; Parameters: 
;;             none.
;; Return Value: List holding computer player's color, and human player's color.
;; Local Variables: 
;;             randomColor, holds atom from random function. This is a random 0 or 1 that chooses the computer player's color.
;; Algorithm: 
;;             1) Get random 0 or 1.
;;			   2) If number is 1, then computer is white. 
;;			   3) Else if number is 0, then computer is black.
;; Assistance Received: none 
;; ********************************************************************* */
(defun computerColor ()
	(let* ( (randomColor (random 1)) )
			(cond 	((= randomColor 1)
						(append (append (list 'w) (list 'human)) (list 'b)))
					((= randomColor 0)
						(append (append (list 'b) (list 'human)) (list 'w))))))

;; /* ********************************************************************* 
;; Function Name: chooseColor 
;; Purpose: Depending on who the first player is, ask first player for their color.
;; Parameters: 
;;             none.
;; Return Value: List holding first player and the first player's color choice.
;; Local Variables: 
;;             none.
;; Algorithm: 
;;             1) If first player is human, then ask human for what color they will play.
;;			   2) If first palyer ic omputer, randomly choose color for computer using computerColor function.
;; Assistance Received: none 
;; ********************************************************************* */
(defun chooseColor(firstPlayer)
	(cond 	((string= (first firstPlayer) 'human)
			(append firstPlayer (readHumanColor)))
			((string= (first firstPlayer) 'computer)
			(append firstPlayer (computerColor )))))

;; /* *********************************************
;; Source Code to draw the game board on the screen
;; ********************************************* */

;; /* ********************************************************************* 
;; Function Name: makeRowForBoard 
;; Purpose: Generates the row for the board.
;; Parameters: 
;;             column, the column index.
;;			   row, the row index.
;;			   boardSize, board size.
;; Return Value: List containing the pieces for that row.
;; Local Variables: 
;;             none.
;; Algorithm: 
;;             1) ...
;; Assistance Received: none 
;; ********************************************************************* */
(defun makeRowForBoard (column row boardSize)
	(cond ((= column 0)
				()             )
				;; First row is white.
				((= row 1)
					(append (list "W")
					(makeRowForBoard (- column 1) row boardSize) )
				)
				;; Place white pieces on second row.
				((and (= row 2) (OR (= column 1) (= column boardSize)))
					(append (list "W")
					(makeRowForBoard (- column 1) row boardSize) )
				)
				;; Place black pieces on last row
				((= row boardSize)
					(append (list "B")
					(makeRowForBoard (- column 1) row boardSize) )        
				)
				;; Place black pieces on second to last row
				((and (= row (- boardSize 1)) (OR (= column 1) (= column boardSize)))
					(append (list "B")
					(makeRowForBoard (- column 1) row boardSize) )
				)
				;; Place regular + pieces
				(t 
				(append (list "+")
				(makeRowForBoard (- column 1) row boardSize) )  )))

;; /* ********************************************************************* 
;; Function Name: makeBoard 
;; Purpose: Generates board with size.
;; Parameters: 
;;             boardSize, the board size.
;;			   constSize, actual board size.
;; Return Value: List of lists that represents board.
;; Local Variables: 
;;             none.
;; Algorithm: 
;;             1) ...
;; Assistance Received: none 
;; ********************************************************************* */
(defun makeBoard (boardSize constSize)
	(cond ((= boardSize 0)
				()             )
				(t 
				(append
				(makeBoard (- boardSize 1) constSize)
				(list (makeRowForBoard constSize boardSize constSize))))))

;; Displays board to user.
(defun displayBoard (board boardlength)
	(cond 		((= (length board) 0)
					(format t "~D ~%" 'S)  
					(format t "~D  " 'W)
					)
				((= boardlength 0)
					(format t "~D ~%" 'N)
					(displayBoard board (+ boardlength 1)))
				(t 
					 (format t "~D ~A ~%" boardlength (first board))
					 (displayBoard (rest board) (+ boardlength 1))
					 (format t "~D " (length board))
				)))

;; Returns row based on row number.
(defun filterRows (board boardlength row)
	(cond (	(= (length board) (- boardlength row) )
			(first board))
		  (t (filterRows (rest board) boardlength row))))

;; Returns column based on colum number.
(defun filterColumns (row boardlength column)
	(cond (	(= (length row) (- boardlength column) )
			(first row))
		  (t (filterColumns (rest row) boardlength column))))

;; Checks if a given piece can be moved.
;; returns piece color as a check, if not piece ,then clearly cannot move - allows computer and human to do their own checks
(defun validPieceToMove (board coordinates)
	;;rest thru rows,
	;; rest thru columns, 
	(cond ((OR (<= (first coordinates) 0) (<= (first (rest coordinates)) 0))
			'x)
		  ((OR (> (first coordinates) (length board)) (> (first (rest coordinates)) (length board)))
			'x)
		  (t
			(filterColumns (filterRows board (+ (length board) 1) (first coordinates)) (+ (length board) 1) (first (rest coordinates))))))

;; Checks if the new coordinates are not occuping piece and direction is not out of bounds
(defun validDirectionToMove (board finalCoordinates)
	(cond ((OR (<= (first finalCoordinates) 0) (<= (first (rest finalCoordinates)) 0))
			'x)
		  ((OR (> (first finalCoordinates) (length board)) (> (first (rest finalCoordinates)) (length board)))
			'x)
		  (t
		   	(filterColumns (filterRows board (+ (length board) 1) (first finalCoordinates)) (+ (length board) 1) (first (rest finalCoordinates))))))


;; Returns row with updated piece at specific column index
(defun updateColumn (row boardlength columnIndex piece)
	(cond ( (= (length row) (- boardlength columnIndex) )
			(append piece (rest row)))
		  (t 
		  	(cons (first row) (updateColumn (rest row) boardlength columnIndex piece)
			))))

(defun updateRow (board boardlength rowIndex row)
	(cond ( (= (length board) (- boardlength rowIndex) )
			(append row (rest board)))
		  (t 
		  	(cons (first board) (updateRow (rest board) boardlength rowIndex row)
			))))

(defun updateCoordinates (board row column piece)
	(updateRow board (+ (length board) 1) row (list (updateColumn (filterRows board (+ (length board) 1) row) (+ (length board) 1) column piece))))

(defun updateBoard (board oldCoordinates NewCoordinates piece)
	;; update new coordinate
	(updateCoordinates (updateCoordinates board (first NewCoordinates) (first (rest NewCoordinates)) piece) (first oldCoordinates) (first (rest oldCoordinates)) (list '+))
	;; remove old coordinate
	)

	

;; // List all the relevant functions here

;; /* *********************************************
;; Source Code to ask the human player for input
;; ********************************************* */

;; Validates choice if yes, then true. Else no.
(defun validYesNo (choice)
	(cond ( (string= choice "Y")
					(print choice)  )
				( (string= choice "N")
					(print choice)  )
				( t 
					(readPlayFromFile) )) )

(defun validPlayAgain (choice)
	(cond ( (string= choice "Y")
					(print choice)  )
				( (string= choice "N")
					(print choice)  )
				( t 
					(readPlayAgain) )) )

;; Validates board size.
(defun validBoardSize (choice)
	(cond ( (= choice 5)
					(print choice)  )
				( (= choice 7)
					(print choice)  )
				( (= choice 9)
					(print choice)  )
				( t 
					(readBoardSize) )) )

;; Validates menu choice.
(defun validMenu (choice)
	(cond ( (= choice 1)
					(list 'save)  )
				( (= choice 2)
					(list 'play)  )
				( (= choice 3)
					(list 'help)  )
				( (= choice 4)
					(list 'quit)  )
				( t 
					(readMenu) )) )

;; Validates direction of piece.
(defun validHumanDirection (choice row column)
	(cond ( (string= choice "NW")
			(append (list (- row 1)) (list (- column 1))))
		  ( (string= choice "NE")
		  	(append (list (- row 1)) (list (+ column 1))))
		  ( (string= choice "SE")
		  	(append (list (+ row 1)) (list (+ column 1))))
		  ( (string= choice "SW")
		  	(append (list (+ row 1)) (list (- column 1))))
		  (t 
		  	(readHumanDirection))))

;; Validates color choice.
(defun validColor (choice)
	(cond ( (string= choice "W")
			(append (append (list 'w) (list 'computer) (list 'b))))
		 ( (string= choice "B")
		 	(append (append (list 'b) (list 'computer) (list 'w))))
		  (t 
		  	(readHumanColor))))

;; Ask user to read if they would like to read game from file.
(defun readPlayFromFile ()
		(princ "Do you want to start a game from a file? (Y/N) ")
		(terpri)
		(validYesNo (read))  )

;; Ask user for filename of save game file.
(defun readFileName()
		(princ "Name of game file: ")
		(terpri)
		(validFile (read-line)))

(defun readSaveFileName()
	(princ "Name of the save file: ")
	(terpri)
	(read-line))

;; Ask user for size of board.
(defun readBoardSize ()
		(princ "Enter size of board (5/7/9): ")
		(terpri)
		(validBoardSize (read))  )

;; Ask user for menu choice.
(defun readMenu ()
		(terpri)
		(princ "1. Save the game.")
		(terpri)
		(princ "2. Make a move.")
		(terpri)
		(princ "3. Ask for help.")
		(terpri)
		(princ "4. Quit the game.")
		(terpri)
		(validMenu (read)))

;; Ask user for color.
(defun readHumanColor ()
		(princ  "What color will you play? (w/b)")
		(terpri)
		(validColor (read)))

;; Ask user for row of piece to move.
(defun readHumanRow ()
	(princ "Enter row of piece to move: ")
	(terpri)
	(list (read)))

;; Ask user for column of piece to move.
(defun readHumanColumn ()
	(princ "Enter column of piece to move: ")
	(terpri)
	(list (read)))

;; Ask user direction to move piece
(defun readHumanDirection (coordinates)
	(princ "Enter direction to move (NW/NE/SE/SW): ")
	(terpri)
	(validHumanDirection (read) (first coordinates) (first (rest coordinates))))

(defun readPlayAgain ()
	(princ "Do you want to play again? (Y/N): ")
	(terpri)
	(validPlayAgain (read)))

;; /* *********************************************
;; Source Code for serialization 
;; ********************************************* */

;; check if file exists
(defun validFile (fileName)
	(probe-file fileName))

;; Converts color identifies (black/white) in file to ones used in game (b/w).
(defun fileColorToGameColor (color)
	(cond 	((string= color "BLACK")
			'b)
			((string= color "WHITE")
			'w)))

;; Convert each board row into board.
;; to do add superpieces
(defun convertBoardRow (row)
	(cond 	( (= (length row) 0)
			())
			( (string= (first row) "B")
			(append (list 'b) (convertBoardRow (rest row))))
			( (string= (first row) "W")
			(append (list 'w) (convertBoardRow (rest row))))
			( (string= (first row) "O")
			(append (list '+) (convertBoardRow (rest row))))))

;; Convert file board to game board.
(defun fileBoardToGameBoard (board)
	(cond 	( (= (length board) 0)
				())
			(t 
				(append (list (convertBoardRow (first board))) (fileBoardToGameBoard  (rest board))))))

;; Saves game to file.
(defun saveToFile(fileName players board currentTurn scores)
	(with-open-file (stream fileName :direction :output :if-exists :supersede)
		(format stream "( ~%")
		(format stream "; Round: ~%")
		;; add actual round #
		(format stream "~S ~%" 3)
		(format stream "; Computer Score: ~%")
		(format stream "~S ~%" (first (rest scores)))
		(format stream "; Computer Color: ~%")
		(format stream "~S ~%" (getPlayerColor players 'computer))
		(format stream "; Human Score: ~%")
		(format stream "~S ~%" (first (rest (rest (rest scores)))))
		(format stream "; Human Color: ~%")
		(format stream "~S ~%" (getPlayerColor players 'human))
		(format stream "; Board: ~%")
		(format stream "~S ~%" board)
		(format stream "; Next Player: ~%")
		(format stream "~S ~%" currentTurn)
		(format stream ")")))


;; Return list of players, board, current player
(defun openFile(fileName)
	;; Invalid file.
	(cond ( (eq fileName nil)
			(princ "Could not open file.")
			(Quit)))

	(with-open-file (stream fileName :direction :input :if-does-not-exist nil)
		(let* ( (file (read stream nil))
				(roundNum (first file))
				(computerScore (first (rest file)))
				(computerColor (first (rest (rest file))))
				(humanScore (first (rest (rest (rest file)))))
				(humanColor (first (rest (rest (rest (rest file))))))
				(board (first (rest (rest (rest (rest (rest file)))))))
				(nextPlayer (first (rest (rest (rest (rest (rest (rest file))))))))
				)

				(format t "Round Number: ~D ~%" roundNum)
				(format t "Computer Score: ~D ~%" computerScore)
				(format t "Computer Color: ~D ~%" (fileColorToGameColor computerColor))
				(format t "Human Score: ~D ~%" humanScore)
				(format t "Human Color: ~D ~%" (fileColorToGameColor humanColor))
				(format t "Board: ~S ~%" (fileBoardToGameBoard board))
				(format t "Next Player: ~D ~%" nextPlayer)

				;; Make players list. Append Board to it. Append next player.
				(append
				(append 
				(append 
				(list (list 'computer (fileColorToGameColor computerColor) 'human (fileColorToGameColor humanColor))) 
				(list (fileBoardToGameBoard board)))
				(list nextPlayer))
				(list 'computer computerScore 'human humanScore))
		)))
	;; (let* (	( inFile (open "game.txt" :direction :input :if-does-not-exist nil))
	;; 		(print (read-line inFile ))
	;; 																			)
	;; 		(close inFile)))

;; gets palyer color given player
(defun getPlayerColor (players currentTurn)
	(cond 	(	(string= currentTurn (first (rest (rest players))) )
				(first (rest (rest (rest players)))))
			(	(string= currentTurn (first players))
				(first (rest players)))))

;; gets opposite player color given player
(defun getOppositePlayerColor (playerColor)
	(cond 	(	(string= player "W") 
				'B)
			(	(string= player "B")
				'W)))
;; get next player
(defun getNextPlayer (players currentTurn)
	(cond 	(	(string= currentTurn (first (rest (rest players))) )
				(first players))
			(	(string= currentTurn (first players))
				(first (rest (rest players))))))


(defun getOpponentColor (players currentTurn)
	(cond 	(	(string= currentTurn (first (rest (rest players))) )
				(first (rest players)))
			(	(string= currentTurn (first players))
				(first (rest (rest (rest players)))))))

;; Help from internet
;; https://stackoverflow.com/questions/2680864/how-to-remove-nested-parentheses-in-lisp#4066110
(defun flatten (l)
  (cond ((null l) nil)
        ((atom (car l)) (cons (car l) (flatten (cdr l))))
        (t (append (flatten (car l)) (flatten (cdr l))))))

;; Get the # of remaining black pieces on the board.
(defun getCountofBlack (board count)
	(cond   ((eq (first board) nil)
			count)
			((string= (first board) "B")
			(getCountOfBlack (rest board) (+ count 1)))
			(t 
			(getCountOfBlack (rest board) count))))

;; Get the # of remaining white pieces on the board.
(defun getCountofWhite (board count)
	(cond   ((eq (first board) nil)
			count)
			((string= (first board) "W")
			(getCountofWhite (rest board) (+ count 1)))
			(t 
			(getCountofWhite (rest board) count))))

;; Returns number of remaining black pieces that have yet to capture the white side.
(defun getWhiteSide (board boardlength numBlack index)
	(cond 	((eq (first board) nil)
			numBlack)
			((AND (<= index (+ boardlength 1)) (string= (first board) "B"))
			(getWhiteSide (rest board) boardlength (- numBlack 1) (+ index 1)))
			((AND (= index (* boardlength 2)) (string= (first board) "B"))
			(getWhiteSide (rest board) boardlength (- numBlack 1) (+ index 1)))
			(t 
			(getWhiteSide (rest board) boardlength numBlack (+ index 1)))))

;; Returns number of remaining white pieces that have yet to capture the black side.
(defun getBlackSide (board boardlength numWhite index)
	(cond 	((eq (first board) nil)
			numWhite)
			((AND (>= index (* boardlength (- boardlength 1))) (string= (first board) "W"))
			(getBlackSide (rest board) boardlength (- numWhite 1) (+ index 1)))
			((AND (= index (* boardlength (- boardlength 2))) (string= (first board) "W"))
			(getBlackSide (rest board) boardlength (- numWhite 1) (+ index 1)))
			((AND (= index (+ (* boardlength (- boardlength 2)) 1)) (string= (first board) "W"))
			(getBlackSide (rest board) boardlength (- numWhite 1) (+ index 1)))
			(t 
			(getBlackSide (rest board) boardlength numWhite (+ index 1)))))

;; checks if there is a winner 
(defun checkwinner(board)
	(cond 	((= (getWhiteSide (flatten board) (length board) (getCountofBlack (flatten board) 0) 1) 0)
			t)
			((= (getBlackSide (flatten board) (length board) (getCountofWhite (flatten board) 0) 1) 0)
			t)
			(t
				())))

(defun countBlackScore (board boardlength score index)
	(cond 	((eq (first board) nil)
			score)
			((AND (= index 1) (string= (first board) "B"))
				(countBlackScore (rest board) boardlength (+ score 3) (+ index 1)))
			((AND (= index 2) (string= (first board) "B"))
				(countBlackScore (rest board) boardlength (+ score 1) (+ index 1)))
			((AND (= index 3) (string= (first board) "B"))
				(countBlackScore (rest board) boardlength (+ score 5) (+ index 1)))
			;; board size is 5
			((AND (AND (= index 4) (string= (first board) "B")) (= boardlength 5))
				(countBlackScore (rest board) boardlength (+ score 1) (+ index 1)))
			((AND (AND (= index 5) (string= (first board) "B")) (= boardlength 5))
				(countBlackScore (rest board) boardlength (+ score 3) (+ index 1)))
			;; board size is 7
			((AND (AND (= index 4) (string= (first board) "B")) (= boardlength 7))
				(countBlackScore (rest board) boardlength (+ score 7) (+ index 1)))
			((AND (AND (= index 5) (string= (first board) "B")) (= boardlength 7))
				(countBlackScore (rest board) boardlength (+ score 5) (+ index 1)))
			((AND (AND (= index 6) (string= (first board) "B")) (= boardlength 7))
				(countBlackScore (rest board) boardlength (+ score 1) (+ index 1)))
			((AND (AND (= index 7) (string= (first board) "B")) (= boardlength 7))
				(countBlackScore (rest board) boardlength (+ score 3) (+ index 1)))
			;; board size is 9
			((AND (AND (= index 4) (string= (first board) "B")) (= boardlength 9))
				(countBlackScore (rest board) boardlength (+ score 7) (+ index 1)))
			((AND (AND (= index 5) (string= (first board) "B")) (= boardlength 9))
				(countBlackScore (rest board) boardlength (+ score 9) (+ index 1)))
			((AND (AND (= index 6) (string= (first board) "B")) (= boardlength 9))
				(countBlackScore (rest board) boardlength (+ score 7) (+ index 1)))
			((AND (AND (= index 7) (string= (first board) "B")) (= boardlength 9))
				(countBlackScore (rest board) boardlength (+ score 5) (+ index 1)))
			((AND (AND (= index 8) (string= (first board) "B")) (= boardlength 9))
				(countBlackScore (rest board) boardlength (+ score 1) (+ index 1)))
			((AND (AND (= index 9) (string= (first board) "B")) (= boardlength 9))
				(countBlackScore (rest board) boardlength (+ score 3) (+ index 1)))
			;; row 2
			((AND (= index (+ boardlength 1)) (string= (first board) "B"))
				(countBlackScore (rest board) boardlength (+ score 1) (+ index 1)))
			((AND (= index (* boardlength 2)) (string= (first board) "B"))
				(countBlackScore (rest board) boardlength (+ score 1) (+ index 1)))
			(t 
			(countBlackScore (rest board) boardlength score (+ index 1)))))

(defun countWhiteScore (board boardlength score index)
	(cond 	((eq (first board) nil)
			score)
			((AND (= index (+ (* boardlength (- boardlength 1)) 1)) (string= (first board) "W"))
				(countWhiteScore (rest board) boardlength (+ score 3) (+ index 1)))
			((AND (= index (+ (* boardlength (- boardlength 1)) 2)) (string= (first board) "W"))
				(countWhiteScore (rest board) boardlength (+ score 1) (+ index 1)))
			((AND (= index (+ (* boardlength (- boardlength 1)) 3)) (string= (first board) "W"))
				(countWhiteScore (rest board) boardlength (+ score 5) (+ index 1)))
			;; board size is 5
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 4)) (string= (first board) "W")) (= boardlength 5))
				(countWhiteScore (rest board) boardlength (+ score 1) (+ index 1)))
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 5)) (string= (first board) "W")) (= boardlength 5))
				(countWhiteScore (rest board) boardlength (+ score 3) (+ index 1)))
			;; board size is 7
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 4)) (string= (first board) "W")) (= boardlength 7))
				(countWhiteScore (rest board) boardlength (+ score 7) (+ index 1)))
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 5)) (string= (first board) "W")) (= boardlength 7))
				(countWhiteScore (rest board) boardlength (+ score 5) (+ index 1)))
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 6)) (string= (first board) "W")) (= boardlength 7))
				(countWhiteScore (rest board) boardlength (+ score 1) (+ index 1)))
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 7)) (string= (first board) "W")) (= boardlength 7))
				(countWhiteScore (rest board) boardlength (+ score 3) (+ index 1)))
			;; board size is 9
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 4)) (string= (first board) "W")) (= boardlength 9))
				(countWhiteScore (rest board) boardlength (+ score 7) (+ index 1)))
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 5)) (string= (first board) "W")) (= boardlength 9))
				(countWhiteScore (rest board) boardlength (+ score 9) (+ index 1)))
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 6)) (string= (first board) "W")) (= boardlength 9))
				(countWhiteScore (rest board) boardlength (+ score 7) (+ index 1)))
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 7)) (string= (first board) "W")) (= boardlength 9))
				(countWhiteScore (rest board) boardlength (+ score 5) (+ index 1)))
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 8)) (string= (first board) "W")) (= boardlength 9))
				(countWhiteScore (rest board) boardlength (+ score 1) (+ index 1)))
			((AND (AND (= index (+ (* boardlength (- boardlength 1)) 9)) (string= (first board) "W")) (= boardlength 9))
				(countWhiteScore (rest board) boardlength (+ score 3) (+ index 1)))
			;; row 2
			((AND (= index (* boardlength (- boardlength 1))) (string= (first board) "W"))
				(countWhiteScore (rest board) boardlength (+ score 1) (+ index 1)))
			((AND (= index (+ (* boardlength (- boardlength 2)) 1)) (string= (first board) "W"))
				(countWhiteScore (rest board) boardlength (+ score 1) (+ index 1)))
			(t 
			(countWhiteScore (rest board) boardlength score (+ index 1)))))

;; Calculates scores for computer and human
(defun calculateScores (board boardlength)
	;; Return white score and black score in that order.
	(list (+ (countWhiteScore board boardlength 0 1) (* (- (+ boardlength 2) (getCountOfBlack board 0)) 5))
	(+ (countBlackScore board boardlength 0 1) (* (- (+ boardlength 2) (getCountofWhite board 0)) 5))))

;; Announces Round scores for computer and human
(defun announceScores (player score)
	(format t "~A scored ~S points. ~%" player score))

;; include tie conclusion
(defun calculateWinner (playerOne scoreOne playerTwo scoreTwo)
	(cond 	((> scoreOne scoreTwo)
				(format t "~A won and is awarded ~S points ~%" playerOne (- scoreOne scoreTwo))
				(list playerOne (- scoreOne scoreTwo)))
			((< scoreOne scoreTwo)
				(format t "~A won and is awarded ~S points ~%" playerTwo (- scoreTwo scoreOne))
				(list playerTwo (- scoreTwo scoreOne)))
			((= scoreOne scoreTwo)
				(format t "There is no clear winner. It is a draw. ~%")
				())))

;; Gets the winner for the round
;; Returns list - player who won, difference in points to be awareded
(defun getWinner(board players)
	(let* ( (scores (calculateScores (flatten board) (length board))))
			;; if first player is white, announce the white score
			(cond 	((string= (first (rest players)) "W")
						(announceScores (first players) (first scores))
						(announceScores (first (rest (rest players))) (first (rest scores)))
						(calculateWinner (first players) (first scores) (first (rest (rest players))) (first (rest scores))))
					((string= (first (rest players)) "B")
						(announceScores (first players) (first (rest scores)))
						(announceScores (first (rest (rest players))) (first scores))
						(calculateWinner (first players) (first (rest scores)) (first (rest (rest players))) (first scores))))))

;; Announces tournament scores for computer and human
(defun announceTournamentScores (player score)
	(format t "~A has ~S points. ~%" player score))

(defun tournamentControl (prevWinner scores)
	(let* ( (tournamentScore (calculateTournamentScore prevWinner scores)))
			(format t "Tournament Scores: ~%")
			(announceTournamentScores (first tournamentScore) (first (rest tournamentScore)))
			(announceTournamentScores (first (rest (rest tournamentScore))) (first (rest (rest (rest tournamentScore)))))
			(let* ( (playAgain (readPlayAgain)))
				(cond 	((string= playAgain "Y")
						(newRound tournamentScore (first prevWinner)))
						((string= playAgain "N")
						(Quit))))
))
	
(defun calculateTournamentScore (roundScores tournamentScores)
	(cond 	((string= (first roundScores) "COMPUTER")
			(list 'computer (+ (first (rest tournamentScores)) (first (rest roundScores))) 'human (first (rest (rest (rest tournamentScores))))))
			((string= (first roundScores) "HUMAN")
			(list 'computer (first (rest tournamentScores)) 'human (+ (first (rest (rest (rest tournamentScores)))) (first (rest roundScores)))))))

;; human player strategy
(defun playHuman (players board scores playerColor)
	(let*( (coordinates (append (readHumanRow) (readHumanColumn) ))
		;; get final coordinates
		(finalCoordinates (readHumanDirection coordinates))
		;; checks if the piece at coordinates is equal to the player color
		(isValidPiece (validPieceToMove board coordinates))
		;; checks if the piece at new coordinates is "+"
		(isValidDirection (validDirectionToMove board finalCoordinates)))
		(format t "start coordinates ~S" coordinates)
		(format t "final coordinates ~S" finalCoordinates)
		(cond 	((AND (string= isValidPiece playerColor) (string= isValidDirection "+") )   
				(playRound players (updateBoard board coordinates finalCoordinates (list playerColor)) 'computer scores))
				(t
				(princ "Not a valid move. Try again.") 
				(playRound players board 'human scores)))))

;; /* *********************************************
;; Source Code to help the computer win the game
;; ********************************************* */
;; // List all the relevant functions here

;; reverse list
;; got help on internet
;; https://stackoverflow.com/questions/34422711/reversing-list-in-lisp#34437069
(defun rev (l)
           (cond
             ((null l) '())
             (T (append (rev (cdr l)) (list (car l))))))

;; return list with closest opponent coordinates
(defun getClosestOpponent (board boardlength opponentColor index)	
	(cond ((string= opponentColor "W")
			(cond   ((string= (first board) "W")
						(list (- (+ boardlength 1) (ceiling index boardlength)) (- (+ boardlength 1) (cond ((= (rem index boardlength) 0) boardlength) (t (rem index boardlength))))))
					(t 
						(getClosestOpponent (rest board) boardlength opponentColor (+ index 1)))))
			((string= opponentColor "B")
			(cond   ((string= (first board) "B")
					(list (ceiling index boardlength) (cond ((= (rem index boardlength) 0) boardlength) (t (rem index boardlength)))))
				(t 
					(getClosestOpponent (rest board) boardlength opponentColor (+ index 1))))))
)

;; check if can block from east, if so, return original coordinates and new coordinates
(defun playDefenseEast(board opponentCoordinates playerColor)
	(print "Playing Defense East")
	
	;; Computer is Black.
	(cond ((string= playerColor "B")
	
		(let* (	(validFinalCoordinate (validDirectionToMove board (list (+ (first opponentCoordinates) 1) (+ (first (rest opponentCoordinates)) 1)) ))
				;; check for available piece in E
				(blockFromEast (validPieceToMove board (list (first opponentCoordinates) (+ (first (rest opponentCoordinates)) 2))))
				;; check for available piece in SE
				(blockFromSouthEast (validPieceToMove board (list (+ (first opponentCoordinates) 2) (+ (first (rest opponentCoordinates)) 2))))
				;; check for available piece in S
				(blockFromSouth (validPieceToMove board (list (+ (first opponentCoordinates) 2) (first (rest opponentCoordinates)) )))
				;; coordinates
				(finalCoordinates (list (+ (first opponentCoordinates) 1) (+ (first (rest opponentCoordinates)) 1))))

		;; Check if the final coordinate is able to moved to, if not return nil.
		(cond ((string/= validFinalCoordinate "+")
				nil)
			(
		;; Return original coordinate and final coordinate - else send nil.
		(cond 	((string= blockFromSouthEast "B")
				(list (list (+ (first opponentCoordinates) 2) (+ (first (rest opponentCoordinates)) 2)) finalCoordinates))
				((string= blockfromSouth "B")
				(list (list (+ (first opponentCoordinates) 2) (first (rest opponentCoordinates))) finalCoordinates))
				((string= blockFromEast "B")
				(list (list (first opponentCoordinates) (+ (first (rest opponentCoordinates)) 2)) finalCoordinates))
				(t 
				nil)
		)))))

	;; Computer is White.
	((string= playerColor "W")
		(let* (	(validFinalCoordinate (validDirectionToMove board (list (- (first opponentCoordinates) 1) (+ (first (rest opponentCoordinates)) 1)) ))
				;; check for available piece in E
				(blockFromEast (validPieceToMove board (list (first opponentCoordinates) (+ (first (rest opponentCoordinates)) 2))))
				;; check for available piece in NE
				(blockFromNorthEast (validPieceToMove board (list (- (first opponentCoordinates) 2) (+ (first (rest opponentCoordinates)) 2))))
				;; check for available piece in N
				(blockFromNorth (validPieceToMove board (list (- (first opponentCoordinates) 2) (first (rest opponentCoordinates)) )))
				;; coordinates
				(finalCoordinates (list (- (first opponentCoordinates) 1) (+ (first (rest opponentCoordinates)) 1))))

		;; Check if the final coordinate is able to moved to, if not return nil.
		(cond ((string/= validFinalCoordinate "+")
				nil)
			(
		;; Return original coordinate and final coordinate - else send nil.
		(cond 	((string= blockFromNorthEast "W")
				(list (list (- (first opponentCoordinates) 2) (+ (first (rest opponentCoordinates)) 2)) finalCoordinates))
				((string= blockFromNorth "W")
				(list (list (- (first opponentCoordinates) 2) (first (rest opponentCoordinates))) finalCoordinates))
				((string= blockFromEast "W")
				(list (list (first opponentCoordinates) (+ (first (rest opponentCoordinates)) 2)) finalCoordinates))
				(t 
				nil)
		)))))))
	
(defun playDefenseWest(board opponentCoordinates playerColor)
	(print "Playing Defense West")
	;; Computer is Black.

	(cond ((string= playerColor "B")
	
		(let* (	(validFinalCoordinate (validDirectionToMove board (list (+ (first opponentCoordinates) 1) (- (first (rest opponentCoordinates)) 1)) ))
				;; check for available piece in W
				(blockFromWest (validPieceToMove board (list (first opponentCoordinates) (- (first (rest opponentCoordinates)) 2))))
				;; check for available piece in SW
				(blockFromSouthWest (validPieceToMove board (list (+ (first opponentCoordinates) 2) (- (first (rest opponentCoordinates)) 2))))
				;; check for available piece in S
				(blockFromSouth (validPieceToMove board (list (+ (first opponentCoordinates) 2) (first (rest opponentCoordinates)) )))
				;; coordinates
				(finalCoordinates (list (+ (first opponentCoordinates) 1) (- (first (rest opponentCoordinates)) 1))))

		;; Check if the final coordinate is able to moved to, if not return nil.
		(cond ((string/= validFinalCoordinate "+")
				nil)
			(
		;; Return original coordinate and final coordinate - else send nil.
		(cond 	((string= blockFromSouthWest "B")
				(list (list (+ (first opponentCoordinates) 2) (- (first (rest opponentCoordinates)) 2)) finalCoordinates "northeast"))
				((string= blockfromSouth "B")
				(list (list (+ (first opponentCoordinates) 2) (first (rest opponentCoordinates))) finalCoordinates "northwest"))
				((string= blockFromWest "B")
				(list (list (first opponentCoordinates) (- (first (rest opponentCoordinates)) 2)) finalCoordinates "southeast"))
				(t 
				nil)
		)))))

	;; Computer is White.
	((string= playerColor "W")
		(let* (	(validFinalCoordinate (validDirectionToMove board (list (- (first opponentCoordinates) 1) (- (first (rest opponentCoordinates)) 1)) ))
				;; check for available piece in W
				(blockFromWest (validPieceToMove board (list (first opponentCoordinates) (- (first (rest opponentCoordinates)) 2))))
				;; check for available piece in NW
				(blockFromNorthWest (validPieceToMove board (list (- (first opponentCoordinates) 2) (- (first (rest opponentCoordinates)) 2))))
				;; check for available piece in N
				(blockFromNorth (validPieceToMove board (list (- (first opponentCoordinates) 2) (first (rest opponentCoordinates)) )))
				;; coordinates
				(finalCoordinates (list (- (first opponentCoordinates) 1) (- (first (rest opponentCoordinates)) 1))))

		;; Check if the final coordinate is able to moved to, if not return nil.
		(cond ((string/= validFinalCoordinate "+")
				nil)
			(
		;; Return original coordinate and final coordinate - else send nil.
		(cond 	((string= blockFromNorthWest "W")
				(list (list (- (first opponentCoordinates) 2) (- (first (rest opponentCoordinates)) 2)) finalCoordinates))
				((string= blockFromNorth "W")
				(list (list (- (first opponentCoordinates) 2) (first (rest opponentCoordinates))) finalCoordinates))
				((string= blockFromWest "W")
				(list (list (first opponentCoordinates) (- (first (rest opponentCoordinates)) 2)) finalCoordinates))
				(t 
				nil)
		)))))))

;; Returns random piece coordinates
(defun getRandomPiece(listOfPieces index)
	(cond 	((= index 0)
				(first listOfPieces))
			(t 
				(getRandomPiece (rest listOfPieces) (- index 1)))))

;; Returns index for getting random piece from list
(defun randomPiece(listOfPieces)
	(random (length listOfPieces)))

;; Returns a list of friendly available pieces.
(defun getFriendlyPieces(board boardlength playerColor index) 
	;; if board has been went thru, return empty list
	(cond ((eq (first board) nil)
			())
		  ((string= (first board) playerColor)
			(append (list (list (ceiling index boardlength) (cond ((= (rem index boardlength) 0) boardlength) (t (rem index boardlength)))))
			(getFriendlyPieces (rest board) boardlength playerColor (+ index 1))))
		  (t
		  	(getFriendlyPieces (rest board) boardlength playerColor (+ index 1)))))
	;; append coordinates to list

(defun makeAttackDecision(board playerColor friendlyPiece)
	(cond ((string= playerColor "B")
			(let* (	;; Valid direction to move northeast.
					(validNorthEast (validDirectionToMove board (list (- (first friendlyPiece) 1) (+ (first (rest friendlyPiece)) 1)) ))
					;; Valid direction to move northwest.
					(validNorthWest (validDirectionToMove board (list (- (first friendlyPiece) 1) (- (first (rest friendlyPiece)) 1)) ))
					;; Northeast coordinates.
					(coordinatesNorthEast (list (- (first friendlyPiece) 1) (+ (first (rest friendlyPiece)) 1)))
					;; Northwest coordinates
					(coordinatesNorthWest (list (- (first friendlyPiece) 1) (- (first (rest friendlyPiece)) 1))))

			;; Check if the final coordinate is able to moved to, if not return nil.
			(cond ((string/= validNorthEast "+")
					nil)
				((string/= validNorthWest "+")
					nil)
				(
			;; Return original coordinate and final coordinate - else send nil.
			(cond 	((string= validNorthEast "+")
					(list friendlyPiece coordinatesNorthEast "northeast"))
					((string= validNorthWest "+")
					(list friendlyPiece coordinatesNorthWest "northwest")))))))
		;; Computer is White.
		  ((string= playerColor "W")
		  	(let* (	;; Valid direction to move southeast.
					(validSouthEast (validDirectionToMove board (list (+ (first friendlyPiece) 1) (+ (first (rest friendlyPiece)) 1)) ))
					;; Valid direction to move southwest.
					(validSouthWest (validDirectionToMove board (list (+ (first friendlyPiece) 1) (- (first (rest friendlyPiece)) 1)) ))
					;; Southeast coordinates.
					(coordinatesSouthEast (list (+ (first friendlyPiece) 1) (+ (first (rest friendlyPiece)) 1)))
					;; Southwest coordinates
					(coordinatesSouthWest (list (+ (first friendlyPiece) 1) (- (first (rest friendlyPiece)) 1))))

			;; Check if the final coordinate is able to moved to, if not return nil.
			(cond ((string/= validSouthEast "+")
					nil)
				((string/= validSouthWest "+")
					nil)
				(
			;; Return original coordinate and final coordinate - else send nil.
			(cond 	((string= validSouthEast "+")
					(list friendlyPiece coordinatesSouthEast "southeast"))
					((string= validSouthWest "+")
					(list friendlyPiece coordinatesSouthWest "southwest")))))))))

;; returns coordinate of piece played and coordinate moved to
(defun playCapture())
	;;

(defun playRetreat(board playerColor listOfPieces)
	(let* ( (coordinates (makeAttackDecision board (getOppositePlayerColor playerColor) (getRandomPiece listOfPieces (randomPiece listOfPieces)))))
		 (cond ((eq coordinates nil)
		 		(playAttack board (getOppositePlayerColor playerColor) listOfPieces))
			   (t 
			   	coordinates))))

;; Loop through list of available pieces, if none can move forward, return t
(defun checkRetreat(board listOfPieces playerColor)
	(cond 	((eq (first listOfPieces) nil)
			t)
			((not (eq (makeAttackDecision board playerColor (first listOfPieces))  nil))
			nil)
			(t
			 (checkRetreat board (rest listOfPieces) playerColor))))

;; Randomly pick piece to attack.
(defun playAttack(board playerColor listOfPieces)
	(let* ( (coordinates (makeAttackDecision board playerColor (getRandomPiece listOfPieces (randomPiece listOfPieces)))))
		 (cond ((eq coordinates nil)
		 		(playAttack board playerColor listOfPieces))
			   (t 
			   	coordinates))))


(defun displayDefense (originalCoordinates finalCoordinates direction)
	(format t "The computer moved the piece at (~S,~S) ~A. ~%" )
	(format t "It wanted to block the human piece at (~S,~S). ~%")
)

(defun playComputer (players board scores playerColor opponentColor)
	;; get opponent's coordinates
	(let* ( (opponentCoordinates 
				(cond ((string= opponentColor "W")
						(getClosestOpponent (rev (flatten board)) (length board) opponentColor 1))
					  ((string= opponentColor "B")
					  	(getClosestOpponent (flatten board) (length board) opponentColor 1))))
			(listOfPieces (getFriendlyPieces (flatten board) (length board) playerColor 1)))
		(print opponentCoordinates)
		(print (playDefenseEast board opponentCoordinates playerColor))
		(print (playDefenseWest board opponentCoordinates playerColor))
		(print listOfPieces)
		(print (playAttack board playerColor listOfPieces))
		(print (checkRetreat board listofPieces playerColor))
		(print "computer strategy")))

;; /* ********************************************************************* 
;; Function Name: playRound 
;; Purpose: Logic for the round. Alternates each player for the turn and holds board state.
;; Parameters: 
;;             none.
;; Return Value: none.
;; Local Variables: 
;;             none.
;; Algorithm: 
;;             1) ...
;; Assistance Received: none 
;; ********************************************************************* */
(defun playRound (players board currentTurn scores)
		(format t "It is ~A's turn. ~%" currentTurn)

		;; Display Board.
		(displayBoard board 0)
		
		;; Check if there is a winner.
		(cond 	((eq (checkwinner board) t)
					(let* ((roundScores (getWinner board players)))
						(tournamentControl roundScores scores))))
		
		;; Display Menu.
		(let*( 	(choice (readMenu))
				(playerColor (getPlayerColor players currentTurn))
				(opponentColor (getOpponentColor players currentTurn)))

				;; Save Game choice
		(cond 	((string= (first choice) 'save)
					(saveToFile (readSaveFileName) players board currentTurn scores))

				;; Play game logic		
				((string= (first choice) 'play)
					(cond 	((string= currentTurn "HUMAN")
								(playHuman players board scores playerColor))
							((string= currentTurn "COMPUTER")
								(playComputer players board scores playerColor opponentColor))))
												
				;; Help mode.
				((string= (first choice) 'help)
					(print "Asking for help"))

				;; Quit game.
				((string= (first choice) 'quit)
					(print "Quiting game")
					(Quit)))))

;; Begins the tournament from loading game from file.
(defun loadGame()
	(let*	(	(fileName (readFileName))
				(gameSave (openFile fileName))
				;;(board)
				
			
			)
				;; Start round from file save.			
				(print (rest (rest (rest gameSave))))
				(playRound (first gameSave) (first (rest gameSave)) (first (rest (rest gameSave))) (rest (rest (rest gameSave))))
			
			))

;; Begins tournament from a new game.
(defun initGame()
	(let* 	(	;; User is asked for board size at the start of round.
				(boardSize (readBoardSize))
				;; Creates board using n size.
				(board (makeBoard boardSize boardSize))
				;; choose first player and board.
				(players (chooseColor (choosefirstPlayer))))
				
				(format t "~A is ~A. ~%" (first players) (first (rest players)))
				(format t "~A is ~A. ~%" (first (rest (rest players))) (first (rest (rest (rest players)))))
				;;make human (first players)
				(playRound players board 'human (list 'computer 0 'human 0))
				
				))

;; Starts new round after finishing old round.
(defun newRound(scores firstPlayer)
	(let* 	(	;; User is asked for board size at the start of round.
				(boardSize (readBoardSize))
				;; Creates board using n size.
				(board (makeBoard boardSize boardSize))
				
				(players (cond 	((string= firstPlayer "COMPUTER")
								(chooseColor (list 'computer)))
								(t 
								(chooseColor (list 'human))))))
				;; choose first player and board.
				
				(format t "~A is ~A. ~%" (first players) (first (rest players)))
				(format t "~A is ~A. ~%" (first (rest (rest players))) (first (rest (rest (rest players)))))

				(playRound players board firstPlayer scores)))

;; Ask user for starting a new game or load a previous one from file.
(let* ( (fileChoice (readPlayFromFile)))
		(cond 	((string= fileChoice "Y")
			 	(loadGame))
				((string= fileChoice "N")
				(initGame))))
