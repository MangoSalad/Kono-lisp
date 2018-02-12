;; ************************************************************
;; * Name:  Philip Glazman                                    *
;; * Project:  Kono - Lisp Implementation                     *
;; * Class:  CMPS 366 Organization of Programming Languages   *
;; * Date:  2/6/2018                                          *
;; ************************************************************

;; TO RUN: sbcl --non-interactive --load program.lsp

;; Random dice roll, return number between 2 and 12. 
(defun randomDice ()
	(+ 2 (random 12)))

;; Process to choose first player
(defun choosefirstPlayer()
	(let * ((humanDice (randomDice)
			(computerDice (randomDice))))
			(cond 	((> humanDice ComputerDice)
					('human))
					((< humanDice ComputerDice)
					('computer))
					((= humanDice ComputerDice)
					(choosefirstPlayer))
			)
	)
)
	

;; /* *********************************************
;; Source Code to draw the game board on the screen
;; ********************************************* */

;; Make row for board.
(defun makeRowForBoard (column row boardSize)
	(cond ((= column 0)
				()             )
				;; First row is white.
				((= row 1)
					(append (list (write 'w))
					(makeRowForBoard (- column 1) row boardSize) )
				)
				;; Place white pieces on second row.
				((and (= row 2) (OR (= column 1) (= column boardSize)))
					(append (list (write 'w))
					(makeRowForBoard (- column 1) row boardSize) )
				)
				;; Place black pieces on last row
				((= row boardSize)
					(append (list (write 'b))
					(makeRowForBoard (- column 1) row boardSize) )        
				)
				;; Place black pieces on second to last row
				((and (= row (- boardSize 1)) (OR (= column 1) (= column boardSize)))
					(append (list (write 'b))
					(makeRowForBoard (- column 1) row boardSize) )
				)
				;; Place regular + pieces
				(t 
				(append (list (write '+))
				(makeRowForBoard (- column 1) row boardSize) )  )))

;; Make board with given size.
(defun makeBoard (boardSize constSize)
	(cond ((= boardSize 0)
				()             )
				(t 
				(append
				(makeBoard (- boardSize 1) constSize)
				(list (makeRowForBoard constSize boardSize constSize)))
				)))

;; Displays board to user.
(defun displayBoard (board boardlength)
	(cond ((= (length board) 0)
					(format t "~D ~%" 'S)  
					(format t "~D  " 'W)
					)
				((= boardlength 0)
					(format t "~D ~%" 'N)
					(displayBoard board (+ boardlength 1)))
				(t 
					 (format t "~D ~S ~%" boardlength (first board))
					 (displayBoard (rest board) (+ boardlength 1))
					 (format t "~D " (length board))
				)))

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

;; Validates menu choice
(defun validMenu (choice)
	(cond ( (= choice 1)
					(print choice)  )
				( (= choice 2)
					(print choice)  )
				( (= choice 3)
					(print choice)  )
				( (= choice 4)
					(print choice)  )
				( t 
					(readMenu) )) )

;; Ask user to read if they would like to read game from file.
(defun readPlayFromFile ()
		(princ "Do you want to start a game from a file? (Y/N) ")
		(terpri)
		(validYesNo (read))  )

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


;; // List all the relevant functions here
;;(readPlayFromFile)
;;(readBoardSize)
;;(randomDice)
;;(print (makeBoard (readBoardSize)))

(let* ((fileChoice (readPlayFromFile))
			(boardSize (readBoardSize))
			(board (makeBoard boardSize boardSize)))
			(terpri)
			(displayBoard board 0)
			(choosefirstPlayer)
			)

;; /* *********************************************
;; Source Code to help the computer win the game
;; ********************************************* */
;; // List all the relevant functions here