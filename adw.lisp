;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; p2-simple.lisp - default (and rather dumb) Coup player for
;; CMSC471 project. Will select relatively arbitrary actions.
;; Coups if 7 coins available, will otherwise select a random action
;;
;; (c) Shawn Squire 2015
;; Version 1.0 - Distributed 10/6/2015
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; PACKAGE DEFINITION
;; You MUST include these two lines, with YOUR OWN TEAM NAME
;; for the defined package.  All of your code "lives" in this
;; package, and it must be the same as the team name that you
;; will use for the tournament!!

(defpackage :adw)
(in-package :adw)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; IMPORTS - symbols (function names) to import from the main
;; game engine package.  Add imports of utility functions here
;; if you choose to use any.
;;

(import '(coup::Moves
					coup::Characters
					coup::CardsPerCharacter coup::CardsPerPlayer
					coup::game-players coup::game-rounds
					coup::player-name coup::player-hand coup::player-faceup
					coup::player-exchange coup::player-handcount
					coup::player-numrounds coup::player-coins
					coup::card-move coup::card-block coup::move-card coup::block-cards
					))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; REQUIRED FUNCTIONS



(defun perform-move (player game) 
	(cond 
		((= (player-coins player) 2) '(coup::ForeignAid))
		((= (player-coins player) 4) '(coup::Tax))
		((= (player-coins player) 7) '(coup::Coup))
		(t (cons (coup::random-elem coup::Moves) (coup::random-elem (remove player (game-players game))))))
)


	
(defun reveal-card (player game)
        (if (= 2 (length (player-hand player)))
                (cond
                        ((eq (car (player-hand player)) 'Duke) 2)
                        ((eq (car (player-hand player)) 'Captain) 2)
                        ((t())))
        (coup::random-elem
                        '(NIL
                                ((1 . 1))
                                ((1 . 2))))))

(defun select-exchange (player game)
	;; Randomly select some combination of cards to exchange
	(if (= 2 (length (player-hand player)))
		(coup::random-elem
			'(NIL
				((1 . 1))
				((1 . 2))
				((2 . 1))
				((2 . 2))
				((1 . 1) (2 . 2))))
		(coup::random-elem
			'(NIL
				((1 . 1))
				((1 . 2))))))

(defun block-move (move player game source &optional target)
	(cond
		((= (game-rounds game) 1) ())
		((< (game-rounds game) 1) (coup::random-elem (coup::block-cards move)))
		(t())))
		
(defun challenge-card (card player game source &optional target)
	(coup::random-bool))

(defun event (e game arguments)
	(cond
		((string= e "MOVE") (case (car arguments)
							('Income "Player (cadr arguments) is using income")
							('ForeignAid "Player (cadr arguments) is using foreign aid")
							('Coup "Player (cadr arguments) is couping (caddr arguments)!")
							('Tax "Player (cadr arguments) is using tax")
							('Assassinate "Player (cadr arguments) is assassinating (caddr arguments)")
							('Exchange "Player (cadr arguments) is using exchange")
							('Steal "Player (cadr arguments) is stealing from (caddr arguments)!")))
		((string= e "SHUFFLE") "Deck is shuffled")
		((string= e "REVEAL") "(car arguments) has shown they have (cadr arguments)")
		((string= e "ELIMINATED") "(car arguments) is totally out!")
		((string= e "CHALLENGE-LOST") "(car arguments) lost that challenge against (cadr arguments) having a (caddr arguments)")
		((string= e "CHALLENGE-WON") "(car arguments) won that challenge against (cadr arguments) having a (caddr arguments)")
		((string= e "BLOCK") "(car arguments) blocked (cadr arguments) (caddr arguments) using their (cadddr arguments)")))
