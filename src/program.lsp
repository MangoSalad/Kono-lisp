;; Seed randomness. Only use of a global var. Find better solution later. 
;; Solution found on internet: https://stackoverflow.com/questions/4034042/random-in-common-lisp-not-so-random
(setf *random-state* (make-random-state t))

;; ************************************************************
;; * Name:  Philip Glazman                                    *
;; * Project:  Kono - Lisp Implementation                     *
;; * Class:  CMPS 366 Organization of Programming Languages   *
;; * Date:  2/6/2018                                          *
;; ************************************************************

;; TO RUN: sbcl --non-interactive --load program.lsp

;; Random dice roll, return number between 2 and 12. 
(defun randomDice ()
	(+ 2 (random 11)))

;; Process to choose first player
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
					(choosefirstPlayer))
			)
	)
)

;; computer chooses random color
;; might be an issue where compuy can only choose 0...
(defun computerColor ()
	(let* ( (randomColor (random 1)) )
			(cond 	((= randomColor 1)
						(append (append (list 'w) (list 'human)) (list 'b)))
					((= randomColor 0)
						(append (append (list 'b) (list 'human)) (list 'w)))
			)
	)
)

;; Choose color
(defun chooseColor(firstPlayer)
	(cond 	((string= (first firstPlayer) 'human)
			(append firstPlayer (readHumanColor)))
			((string= (first firstPlayer) 'computer)
			(append firstPlayer (computerColor ))))
)

;; Alternative between player and play round.
(defun playRound (players board currentTurn)
		(format t "It is ~A's turn. ~%" currentTurn)
		(displayBoard board 0)
		;;(check winner)
		(readMenu)
		(cond 
			((string= currentTurn (first (rest (rest players))) )
			 	(playRound players board (first players)))
			((string= currentTurn (first players))
				(playRound players board (first (rest (rest players)))))
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
					(append (list 'w)
					(makeRowForBoard (- column 1) row boardSize) )
				)
				;; Place white pieces on second row.
				((and (= row 2) (OR (= column 1) (= column boardSize)))
					(append (list 'w)
					(makeRowForBoard (- column 1) row boardSize) )
				)
				;; Place black pieces on last row
				((= row boardSize)
					(append (list 'b)
					(makeRowForBoard (- column 1) row boardSize) )        
				)
				;; Place black pieces on second to last row
				((and (= row (- boardSize 1)) (OR (= column 1) (= column boardSize)))
					(append (list 'b)
					(makeRowForBoard (- column 1) row boardSize) )
				)
				;; Place regular + pieces
				(t 
				(append (list '+)
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
					(Quit)  )
				( t 
					(readMenu) )) )

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

;; // List all the relevant functions here
;;(readPlayFromFile)
;;(readBoardSize)
;;(randomDice)
;;(print (makeBoard (readBoardSize)))

;; init game new
(let* 	(	;; User is asked to resume game from text file.
			(fileChoice (readPlayFromFile))
			;; User is asked for board size at the start of round.
			(boardSize (readBoardSize))
			;; Creates board using n size.
			(board (makeBoard boardSize boardSize))
			;; choose first player and board.
			(players (chooseColor (choosefirstPlayer))))

			(playRound players board (first players))
			
			)

;; display board.
;; (displayBoard board 0)


;; /* *********************************************
;; Source Code to help the computer win the game
;; ********************************************* */
;; // List all the relevant functions here