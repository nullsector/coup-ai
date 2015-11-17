;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; adw.lisp - group created player. Based on p2-simple.lisp
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


;;Function: perform-move
;;Function will bluff cards in an attempt to quickly acrue coins.
;;However, every 3 rounds the agent will switch to a much slower income-based moveset
;;This is in case any one of our bluffs is blocked, we will not infinitely try to bluff a move that will be blocked
(defun perform-move (player game)
  (cond
   ;;back to bluff mode
   ((> (game-rounds game) 14) (cond
                               ((> (player-coins player) 6) '(coup::Coup))
                               ;;as we get closer to our 7 coins, slow down and use income as to remove any chance of being blocked
                               ((> (player-coins player) 5) '(coup::Income))
                               ;;early rounds, we can bluff in an attempt to very quickly draw in coins
                               ((> (player-coins player) 2) '(coup::Tax))
                               (t '(coup::ForeignAid))))
   ;;income mode. switch cases every 3 rounds in case a bluff or move is being blocked
   ((> (game-rounds game) 11) (cond
                               ((> (player-coins player) 7) '(coup::Coup))
                               ((> (player-coins player) 3) '(coup::ForeignAid))
                               (t'(coup::Income))))
   ;;bluff mode
   ((> (game-rounds game) 8) (cond
                              ((> (player-coins player) 6) '(coup::Coup))
                              ((> (player-coins player) 5) '(coup::Income))
                              ((> (player-coins player) 2) '(coup::Tax))
                              (t '(coup::ForeignAid))))
   ;;income
   ((> (game-rounds game) 5) (cond
                              ((> (player-coins player) 7) '(coup::Coup))
                              ((> (player-coins player) 3) '(coup::ForeignAid))
                              (t'(coup::Income))))
   ;;bluff
   ((< (game-rounds game) 6) (cond
                              ((> (player-coins player) 6) '(coup::Coup))
                              ((> (player-coins player) 5) '(coup::Income))
                              ((> (player-coins player) 2) '(coup::Tax))
                              (t '(coup::ForeignAid))))
   ;;base case. un-needed, because conditionals should cover all cases

   ( (t'()))

  )
)

;;Function: reveal-card
;;Standard function, just reveals one of the cards in hand
(defun reveal-card (player game) (+ (random 2) 1))

;;Function: select-exchange
;;select some order of cards to exchange
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

;;Function: block-move
;;in phase 1, we have not implemented knowledge bases
;;as a result, our block will bluff in accordance with our perform move bluff (which is that we have a Duke)
;;in phase 2, we will use our knowledge base for smarter bluffs and consider when to actually use the cards in hand
(defun block-move (move player game source &optional target)
  ;;first round will be for "information gathering" will be expanded in phase 2
  (cond
   ((= (game-rounds game) 1) ())
   ((< (game-rounds game) 1)
    ;;after round 1, we can begin our bluff
    (cond
     ((eq move 'ForeignAid) '(Duke))
     (t'(NIL))))
   (t())))

;;Function: challenge-card
;;Function will not be based on bluffs but rather on actual knowledge bases
;;Since we do not have logic bases in phase 1, this funciton does nothing right now
(defun challenge-card (card player game source &optional target))
  ;;No challenges for Phase I since we have no knowledge base. No need to shoot ourselves in the foot

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
