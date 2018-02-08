;; ************************************************************
;; * Name:  Philip Glazman                                    *
;; * Project:  Kono - Lisp Implementation                     *
;; * Class:  CMPS 366 Organization of Programming Languages   *
;; * Date:  2/6/2018                                          *
;; ************************************************************

;; TO RUN: sbcl --non-interactive --load program.lsp

;; (defun function ( first second ) 
;;    (cond ( ( first-condition 
;;              first-action )
;;            ( second-condition
;;              second-action )
;;            ( t
;;              default-action ) )) )

;; Validates choice if yes, then true. Else no.
(defun validYesNo (choice)
  (cond ( (/= choice "y")
          (readPlayFromFile)  )
        ( (/= choice "n")
          (readPlayFromFile)  )
        ( t 
          choice )) )

;; Ask user to read if they would like to read game from file.
(defun readPlayFromFile ()
    (princ "Do you want to start a game from a file? (y/n) ")
    (terpri)
    (validYesNo (read))  )

;; Ask user for size of board.
;; (defun readBoardSize ()
;;     (princ "Enter size of board (5/7/9): ")
;;     (terpri)
;;     (valid (read))  )

(readPlayFromFile)

;; /* *********************************************
;; Source Code to draw the game board on the screen
;; ********************************************* */
;; // List all the relevant functions here

;; /* *********************************************
;; Source Code to ask the human player for input
;; ********************************************* */
;; // List all the relevant functions here

;; /* *********************************************
;; Source Code to help the computer win the game
;; ********************************************* */
;; // List all the relevant functions here
(print '1)