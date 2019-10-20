;;; rogue.el --- Simple roguelike within Emacs. -*- lexical-binding: t; -*-

;;; Commentary:
;;; Not participating in 7drl, but it inspired me to create something, anyway.

;;; License: GNU GPLv3

;;; Code:

;;; Some parts of the code could conceivably benefit from proper OO programming,
;;; namely using eieio.el. However, it was skipped for a more familiar
;;; list-based approach which allows for sufficient abstractions.
;;;
;;; Also, while it may not seem wise, an attempt is made to steer clear of
;;; any cl.el features, raising the need to implement a few basic utilities.
;;; This is not a sign of disdain for Common Lisp, rather than a choice to
;;; explore the natural capabilities of Emacs Lisp.

(require 'seq)

;;; Display ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +rogue-player-symbol+ "@"
  "Representation of the player on the board.")

;;; Game Parameters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; General config

(defconst +rogue-buffer-name+ "A simple roguelike"
  "The name of the game buffer.")

;; Display

(defconst +rogue/horizo-wall+ "━" "Horizontal wall segment.")
(defconst +rogue/vertic-wall+ "┃" "Vertical wall segment.")
(defconst +rogue/tlcorn-wall+ "┏" "Top left corner wall segment.")
(defconst +rogue/trcorn-wall+ "┓" "Top right corner wall segment.")
(defconst +rogue/blcorn-wall+ "┗" "Bottom left corner wall segment.")
(defconst +rogue/brcorn-wall+ "┛" "Bottom right corner wall segment.")
(defconst +rogue/door-right+ "┣" "Right door segment.")
(defconst +rogue/door-left+ "┫" "Left door segment.")
(defconst +rogue/door-bottom+ "┳" "Bottom door segment.")
(defconst +rogue/door-top+ "┻" "Top door segment.")
(defconst +rogue/stairs-up+ "⇑" "Stairs to another level.")
(defconst +rogue/stairs-down+ "⇓" "Stairs to another level.")
(defconst +rogue/empty-tile+ " " "The representation of an empty dungeon tile.")

;; Player related

(defvar *rogue-player-max-hp* 0
  "The maximum hitpoints of the player.")
(defvar *rogue-player-current-hp* 0
  "The player's current hitpoints.")

(defvar *rogue-player-position* nil
  "The position of the player within the current room.")
(defvar *rogue-player-armor* nil
  "The currently equipped items.")
(defvar *rogue-player-inventory* nil
  "The player's inventory.")
(defvar *rogue-player-weapon* nil
  "The currently held weapon.")
(defvar *rogue-player-spell* nil
  "The currently active spell.")

(make-variable-buffer-local '*rogue-player-max-hp*)
(make-variable-buffer-local '*rogue-player-position*)
(make-variable-buffer-local '*rogue-player-armor*)
(make-variable-buffer-local '*rogue-player-inventory*)
(make-variable-buffer-local '*rogue-player-weapon*)
(make-variable-buffer-local '*rogue-player-spell*)

;; Spells

(defvar *rogue/spell/heal* nil
  "A healing spell")

;; Level related

(defconst +rogue-rooms-per-level+ 25
  "The number of rooms per level. Must be less than 100.")
(defconst +rogue-num-levels+ 3
  "The number of levels in the game.")
(defconst +rogue-room-min-side-length+ 13
  "Minimal side length of a dungeon room.")
(defconst +rogue-room-max-side-length+ 21
  "Maximal side length of a dungeon room.")

(defvar *rogue-current-room* nil
  "The currently occupied room.")
(defvar *rogue-current-level* nil
  "The currently occupied level.")
(defvar *rogue-levels* nil
  "The levels for the game.")

(make-variable-buffer-local '*rogue-current-room*)
(make-variable-buffer-local '*rogue-current-level*)
(make-variable-buffer-local '*rogue-levels*)

;; Fight related

(defvar *rogue-current-monster* nil "The monster currently in battle.")
(defvar *rogue-fight-log* nil "The log of the current fight's events.")

;;; Game Config ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst rogue/dungeon-keymap
  (let ((map (make-sparse-keymap 'rogue/dungeon-keymap)))
    (define-key map [left] #'rogue/player/move-left)
    (define-key map [right] #'rogue/player/move-right)
    (define-key map [up] #'rogue/player/move-up)
    (define-key map [down] #'rogue/player/move-down)
    (define-key map "i" #'rogue/player/access-inventory)
    (define-key map "q" #'rogue/quit)
    map))

;;; Gameplay ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue ()
  "Play a game of Rogue."
  (interactive)
  (switch-to-buffer +rogue-buffer-name+)
  (rogue-mode))

(define-derived-mode rogue-mode
  special-mode
  "Rogue"
  "A mode for playing a roguelike."
  (use-local-map rogue/dungeon-keymap)
  (set-buffer-multibyte t)
  (rogue/init))

(defun rogue/init ()
  "Initialize the rooms, monsters, etc."
  (interactive)
  (setq *rogue-levels* (rogue/levels/make-all +rogue-num-levels+))
  (setq *rogue-current-level* (assoc 1 *rogue-levels*))
  (setq *rogue-current-room* (rogue/level/room *rogue-current-level* 101))
  (setq *rogue-player-position* (rogue/room/center *rogue-current-room*))
  (setq *rogue-player-max-hp* 10)
  (setq *rogue-player-current-hp* *rogue-player-max-hp*)
  (setq *rogue-player-armor* nil)
  (setq *rogue-player-inventory* nil)
  (setq *rogue-player-weapon* (rogue/weapon/make 'SWORD))
  (setq *rogue-player-spell* *rogue/spell/heal*)
  (setq *rogue-current-monster* nil)
  (setq *rogue-fight-log* nil)
  (rogue/spells/init)
  (rogue/draw/dungeon))

(defun rogue/quit ()
  "Quit the game of Rogue."
  (interactive)
  (when (yes-or-no-p "Do you really wish to quit Rogue?")
    (kill-buffer +rogue-buffer-name+)))

(defun rogue/player/move-done ()
  "The function to call after the player made their move."
  (rogue/monsters/move)
  (if (rogue/player/monster-collision-p)
      (rogue/fight/start)
    (rogue/draw/dungeon)))

;;; Graphics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/draw/dungeon ()
  "Draw the dungeon screen."
  (setq-local buffer-read-only nil)
  (erase-buffer)
  (rogue/draw/stat-header)
  (let* ((dims (rogue/room/dims *rogue-current-room*))
         (monsters (rogue/room/monsters *rogue-current-room*))
         (door-places (mapcar #'rogue/door/placement
                              (rogue/room/doors *rogue-current-room*)))
         (top-pad (/ (- (rogue/dim/y dims)
                        +rogue-room-max-side-length+)
                     2)))
    (dotimes (_ top-pad)
      (newline))
    (dotimes (n (rogue/dim/y dims))
      (rogue/draw/dungeon-row n dims monsters door-places)))
  (setq-local buffer-read-only t))

(defun rogue/draw/fight ()
  "Draw the fight screen for fighting the monster at the current position."
  (setq-local buffer-read-only nil)
  (erase-buffer)
  (rogue/draw/stat-header)
  (let* ((monster *rogue-current-monster*)
         (type (rogue/monster/type monster)))
    (insert (format "Monster:    %s\n" type))
    (insert (format "Monster HP: %d/%d\n"
                    (rogue/monster/hp monster)
                    (rogue/monster/max-hp monster))))
  (newline)
  (dolist (log-line *rogue-fight-log*)
    (insert log-line "\n"))
  (setq-local buffer-read-only t))

(defun rogue/draw/stat-header ()
  "Print a general stat header."
  (insert (format "Level:      %d\n"
                  (rogue/level/number *rogue-current-level*)))
  (insert (format "Room:       %d\n"
                  (rogue/room/number *rogue-current-room*)))
  (insert (format "Hitpoints:  %d/%d\n\n"
                  *rogue-player-current-hp*
                  *rogue-player-max-hp*)))

(defun rogue/draw/dungeon-row (n room-dims monsters door-places)
  "Draw line N of a room of size ROOM-DIMS, with MONSTERS and DOOR-PLACES."
  (let* ((left-pad (/ (- +rogue-room-max-side-length+
                         (rogue/dim/x room-dims))
                      2))
         (padding (make-string left-pad ?\ ))
         (dim-x (rogue/dim/x room-dims))
         (dim-y (rogue/dim/y room-dims))
         (has-north-door (seq-contains door-places 'NORTH))
         (has-east-door (seq-contains door-places 'EAST))
         (has-south-door (seq-contains door-places 'SOUTH))
         (has-west-door (seq-contains door-places 'WEST))
         (horiz-door-line (/ (1- dim-y) 2)))
    (insert padding)
    (cond
     ;; Horizontal walls.
     ((= n 0)
      (rogue/draw/dungeon-wall-top dim-x has-north-door))
     ((= n (1- dim-y))
      (rogue/draw/dungeon-wall-bot dim-x has-south-door))
     ;; Possible door elements at west and east end.
     ((= n (1- horiz-door-line))
      (insert (if has-west-door +rogue/door-top+ +rogue/vertic-wall+))
      (rogue/draw/dungeon-interior n monsters)
      (insert (if has-east-door +rogue/door-top+ +rogue/vertic-wall+)))
     ((= n horiz-door-line)
      (insert (if has-west-door +rogue/empty-tile+ +rogue/vertic-wall+))
      (rogue/draw/dungeon-interior n monsters)
      (insert (if has-east-door +rogue/empty-tile+ +rogue/vertic-wall+)))
     ((= n (1+ horiz-door-line))
      (insert (if has-west-door +rogue/door-bottom+ +rogue/vertic-wall+))
      (rogue/draw/dungeon-interior n monsters)
      (insert (if has-east-door +rogue/door-bottom+ +rogue/vertic-wall+)))
     ;; Normal segment, no doors.
     (t (insert +rogue/vertic-wall+)
        (rogue/draw/dungeon-interior n monsters)
        (insert +rogue/vertic-wall+)))
    (newline)))

(defun rogue/draw/dungeon-interior (n monsters)
  "Draw line N of the inside of the current room.

Place the player and MONSTERS where appropriate."
  (let ((y n)
        (room-dims (rogue/room/dims *rogue-current-room*)))
    (dotimes (k (- (rogue/dim/x room-dims)
                   2))
      (let* ((x (+ k 1))
             (pos (rogue/pos x y))
             (m (rogue/monsters/monster-at pos monsters)))
        (cond
         ((equal pos *rogue-player-position*)
          (insert +rogue-player-symbol+))
         (m (insert (rogue/monster/symbol m)))
         (t (insert +rogue/empty-tile+)))))))

(defun rogue/draw/dungeon-wall-top (len has-door)
  "Draw a horizontal wall of length LEN.

If HAS-DOOR is non-nil, add a door in its center."
  (if has-door
      (let ((half-wall (apply #'concat (make-list (/ (- len 5) 2)
                                                +rogue/horizo-wall+))))
        (insert +rogue/tlcorn-wall+ half-wall +rogue/door-left+
                +rogue/empty-tile+
                +rogue/door-right+ half-wall +rogue/trcorn-wall+))
    (let ((full-wall (apply #'concat (make-list (- len 2)
                                                +rogue/horizo-wall+))))
      (insert +rogue/tlcorn-wall+ full-wall +rogue/trcorn-wall+))))

(defun rogue/draw/dungeon-wall-bot (len has-door)
  "Draw a horizontal wall of length LEN.

If HAS-DOOR is non-nil, add a door in its center."
  (if has-door
      (let ((half-wall (apply #'concat (make-list (/ (- len 5) 2)
                                                  +rogue/horizo-wall+))))
        (insert +rogue/blcorn-wall+ half-wall +rogue/door-left+
                +rogue/empty-tile+
                +rogue/door-right+ half-wall +rogue/brcorn-wall+))
    (let ((full-wall (apply #'concat (make-list (- len 2)
                                                +rogue/horizo-wall+))))
      (insert +rogue/blcorn-wall+ full-wall +rogue/brcorn-wall+))))

;;; Players ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/player/access-inventory ()
  "Access the player's inventory."
  (interactive)
  (error "Not yet implemented: Inventory"))

(defun rogue/player/enter-door ()
  "Enter a door."
  (let* ((door (rogue/player/door-collision-p))
         (current-room-no (rogue/room/number *rogue-current-room*))
         (target-room (rogue/level/room *rogue-current-level*
                                        (rogue/door/target door)))
         (dims (rogue/room/dims target-room))
         (center (rogue/dim/center (rogue/room/dims target-room)))
         (back-door (seq-find (lambda (door)
                                (= (rogue/door/target door) current-room-no))
                              (rogue/room/doors target-room)))
         (target-placement (rogue/door/placement back-door)))
    (setq *rogue-player-position*
          (cond
           ((equal target-placement 'NORTH)
            (rogue/pos (rogue/pos/x center) 1))
           ((equal target-placement 'SOUTH)
            (rogue/pos (rogue/pos/x center)
                       (- (rogue/dim/y dims) 2)))
           ((equal target-placement 'WEST)
            (rogue/pos 1 (rogue/pos/y center)))
           ((equal target-placement 'EAST)
            (rogue/pos (- (rogue/dim/x dims) 2) (rogue/pos/y center)))))
    (setq *rogue-current-room* target-room))
  (rogue/draw/dungeon))

(defun rogue/player/move-left ()
  "Move the player to the left."
  (interactive)
  (rogue/pos/x-dec *rogue-player-position*)
  (cond
   ((rogue/player/wall-collision-p)
    (rogue/pos/x-inc *rogue-player-position*)
    (message "Ouch!"))
   (t (rogue/player/check-collisions))))

(defun rogue/player/move-right ()
  "Move the player to the right."
  (interactive)
  (rogue/pos/x-inc *rogue-player-position*)
  (cond
   ((rogue/player/wall-collision-p)
    (rogue/pos/x-dec *rogue-player-position*)
    (message "Ouch!"))
   (t (rogue/player/check-collisions))))

(defun rogue/player/move-up ()
  "Move the player up."
  (interactive)
  (rogue/pos/y-dec *rogue-player-position*)
  (cond
   ((rogue/player/wall-collision-p)
    (rogue/pos/y-inc *rogue-player-position*)
    (message "Ouch!"))
   (t (rogue/player/check-collisions))))

(defun rogue/player/move-down ()
  "Move the player down."
  (interactive)
  (rogue/pos/y-inc *rogue-player-position*)
  (cond
   ((rogue/player/wall-collision-p)
    (rogue/pos/y-dec *rogue-player-position*)
    (message "Ouch!"))
   (t (rogue/player/check-collisions))))

(defun rogue/player/check-collisions ()
  "Check the current player position for possible collisions and react."
  (cond
   ((rogue/player/monster-collision-p)
    (rogue/fight/start))
   ((rogue/player/door-collision-p)
    (rogue/player/enter-door))
   (t (rogue/player/move-done))))

(defun rogue/player/border-collision-p ()
  "Whether the player collided with the current room's borders."
  (let* ((x (rogue/pos/x *rogue-player-position*))
         (y (rogue/pos/y *rogue-player-position*))
         (dims (rogue/room/dims *rogue-current-room*))
         (dim-x (rogue/dim/x dims))
         (dim-y (rogue/dim/y dims)))
    (or (= x 0)
        (= y 0)
        (= x (1- dim-x))
        (= y (1- dim-y)))))

(defun rogue/player/wall-collision-p ()
  "Check whether the player collided with a wall."
  (and (rogue/player/border-collision-p)
       (not (rogue/player/door-collision-p))))

(defun rogue/player/monster-collision-p ()
  "Check whether the player collided with any monsters."
  (let* ((monsters (rogue/room/monsters *rogue-current-room*))
         (at-player-pos
          (seq-filter
           (lambda (monster)
             (rogue/pos/within-one (rogue/monster/pos monster)
                                   *rogue-player-position*))
           monsters)))
    (unless (null at-player-pos)
      (car at-player-pos))))

(defun rogue/player/door-collision-p ()
  "Check whether the player collided with a door.

Returns the corresponding door if one exists, nil otherwise."
  (let* ((x (rogue/pos/x *rogue-player-position*))
         (y (rogue/pos/y *rogue-player-position*))
         (dims (rogue/room/dims *rogue-current-room*))
         (center (rogue/dim/center dims))
         (x-centered (= x (rogue/pos/x center)))
         (y-centered (= y (rogue/pos/y center)))
         (placement (cond
                     ((= x 0)
                      (when y-centered 'WEST))
                     ((= x (1- (rogue/dim/x dims)))
                      (when y-centered 'EAST))
                     ((= y 0)
                      (when x-centered 'NORTH))
                     ((= y (1- (rogue/dim/y dims)))
                      (when x-centered 'SOUTH))
                     (t nil))))
    (assoc placement (rogue/room/doors *rogue-current-room*))))

(defun rogue/player/reduce-hp (amount)
  "Reduce the player's hitpoints by AMOUNT."
  (setq *rogue-player-current-hp*
        (- *rogue-player-current-hp* amount)))

(defun rogue/player/alive-p ()
  "Whether the player is currently alive."
  (> *rogue-player-current-hp* 0))

;;; Fight ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst rogue/fight-keymap
  (let ((map (make-sparse-keymap 'rogue/fight-keymap)))
    (define-key map "a" #'rogue/fight/weapon-attack)
    (define-key map "s" #'rogue/fight/cast-spell)
    (define-key map "q" #'rogue/fight/quit)
    (define-key map [up] #'rogue/fight/cannot-move)
    (define-key map [right] #'rogue/fight/cannot-move)
    (define-key map [down] #'rogue/fight/cannot-move)
    (define-key map [left] #'rogue/fight/cannot-move)
    map))

(defun rogue/fight/start ()
  "Enter fight mode."
  (let ((monster (rogue/player/monster-collision-p)))
    (unless monster (error "Call to FIGHT with no monster present"))
    (setq *rogue-current-monster* monster)
    (push (format "You are now fighting %s" (rogue/monster/type monster))
          *rogue-fight-log*))
  (use-local-map rogue/fight-keymap)
  (rogue/draw/fight)
  (message "Started fighting"))

(defun rogue/fight/quit ()
  "Exit fight mode."
  (interactive)
  (cond ((not (rogue/player/alive-p))
         (rogue/quit))
        ((rogue/monster/alive-p *rogue-current-monster*)
         (error "Cannot leave an ongoing fight"))
        (t
         (message "You defeated %s"
                  (rogue/monster/type *rogue-current-monster*))
         (setq *rogue-current-monster* nil)
         (setq *rogue-fight-log* nil)
         (use-local-map rogue/dungeon-keymap)
         (rogue/draw/dungeon))))

(defun rogue/fight/cannot-move ()
  "Signal the player that moving is disabled during a fight."
  (interactive)
  (message "Cannot move -- Fighting"))

(defun rogue/fight/move-done ()
  "Evaluations after attacking a monster."
  (if (rogue/monster/alive-p *rogue-current-monster*)
     (rogue/fight/monster-attack)
    (rogue/fight/loot)))

(defun rogue/fight/loot ()
  "Look for loot among the remains of a slain monster."
  (rogue/draw/fight)
  (message "Press q to exit the fight screen")
  'TODO)

(defun rogue/fight/weapon-attack ()
  "Attack with the current weapon."
  (interactive)
  (when (rogue/player/alive-p)
    (if *rogue-current-monster*
        (if (rogue/monster/alive-p *rogue-current-monster*)
            (progn
              (let ((dmg (rogue/weapon/dmg *rogue-player-weapon*))
                    (mtype (rogue/monster/type *rogue-current-monster*)))
                (rogue/monster/reduce-hp *rogue-current-monster* dmg)
                (push (format "You hit %s with %s for %d damage."
                              mtype
                              (rogue/weapon/type *rogue-player-weapon*)
                              dmg)
                      *rogue-fight-log*)
                (when (not (rogue/monster/alive-p *rogue-current-monster*))
                  (push (format "You killed %s." mtype)
                        *rogue-fight-log*)))
              (rogue/draw/fight)
              (rogue/fight/move-done))
          (message "The monster is already dead"))
      (error "Not in a fight"))))

(defun rogue/fight/cast-spell ()
  "Attack with the currently active spell."
  (interactive)
  (when (rogue/player/alive-p)
    (if *rogue-current-monster*
        (if *rogue-player-spell*
            (progn
              (push (format "You cast %s"
                            (rogue/spell/name *rogue-player-spell*))
                    *rogue-fight-log*)
              (rogue/spell/cast-in-combat *rogue-player-spell*)
              (rogue/draw/fight)
              (rogue/fight/move-done))
          (message "No spell selected"))
      (error "Not in a fight"))))

(defun rogue/fight/monster-attack ()
  "The current monster attacks the player."
  (let ((dmg (rogue/monster/damage *rogue-current-monster*)))
    (rogue/player/reduce-hp dmg)
    (push (format "%s hit you for %d damage."
                  (rogue/monster/type *rogue-current-monster*)
                  dmg)
          *rogue-fight-log*)
    (if (not (rogue/player/alive-p))
        (progn
          (push "You have died." *rogue-fight-log*)
          (rogue/draw/fight)
          (if (yes-or-no-p "Play again?")
              (rogue)
            (kill-buffer +rogue-buffer-name+)))
      (rogue/draw/fight))))

;;; Levels ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/levels/make-all (num-levels)
  "Create NUM-LEVELS levels and connect them."
  (let ((levels (mapcar #'rogue/levels/make-one
                            (rogue/util/range 1 num-levels))))
    ;; TODO
    levels))

(defun rogue/levels/make-one (level-number)
  "Create and populate level LEVEL-NUMBER."
  (let* ((room-numbers (mapcar (lambda (x) (+ x (* 100 level-number)))
                               (rogue/util/range 1 +rogue-rooms-per-level+)))
         (shuf (rogue/util/shuffle-list room-numbers))
         (partitioned (rogue/util/sublists 2 3 shuf))
         (rooms-unsorted
          (cons (rogue/rooms/make level-number
                                  (caar partitioned)
                                  (list (cadar partitioned)))
                (mapcar (lambda (numbers)
                          (rogue/rooms/make level-number
                                            (cadr numbers)
                                            (cons (car numbers) (cddr numbers))))
                        partitioned)))
         (rooms (sort rooms-unsorted
                      (lambda (x y)
                        (< (car x) (car y))))))
    (cons level-number rooms)))

(defun rogue/level/number (level)
  "The number of LEVEL."
  (car level))

(defun rogue/level/all-rooms (level)
  "The list of rooms in LEVEL."
  (cdr level))

(defun rogue/level/room (level room-number)
  "Get the room with number ROOM-NUMBER from LEVEL."
  (assoc room-number (rogue/level/all-rooms level)))

(defun rogue/level/first-room (level)
  "The first room of LEVEL."
  (let ((room-number (+ 1 (* 100 (rogue/level/number level)))))
    (rogue/level/room level room-number)))

(defun rogue/level/last-room (level)
  "The last room of LEVEL."
  (let ((room-number (+ +rogue-rooms-per-level+
                        (* 100 (rogue/level/number level)))))
    (rogue/level/room level room-number)))

;;; Rooms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/rooms/side-len ()
  "One random side length of a room.

Possible sizes are delimited by +ROGUE-MIN-SIDE-LENGTH+ and
+ROGUE-MAX-SIDE-LENGTH+. Always returns an odd number for easier symmetry."
  (let ((addend
         (/ (- +rogue-room-max-side-length+ +rogue-room-min-side-length+) 2)))
    (+ (* 2 (/ +rogue-room-min-side-length+ 2)) (* 2 (random addend)) 1)))

(defun rogue/rooms/monsters (level)
  "Random monsters for a room in LEVEL."
  (cond ((= level 1)
         (seq-random-elt '((OGRE)
                           (SKELETON SKELETON)
                           (GOAT GOAT SKELETON))))
        ((= level 2)
         (seq-random-elt '((OGRE SKELETON) (CENTAUR))))
        ((= level 3)
         ;; Pick any two
         (let ((available '(CENTAUR MEDUSA DRAGON)))
           (list (seq-random-elt available)
                 (seq-random-elt available))))
        (t (error "No monsters defined for level number %d" level))))

(defun rogue/rooms/make (level room-number adjacent-rooms)
  "Create room ROOM-NUMBER in LEVEL with and doors leading to ADJACENT-ROOMS.

Dimensions, monsters, and objects are chosen randomly in accordance with the
relevant variables."
  (let* ((dimensions (rogue/dim (rogue/rooms/side-len)
                                (rogue/rooms/side-len)))
         (doors (rogue/doors/place adjacent-rooms))
         (monster-types (rogue/rooms/monsters level))
         (monsters (rogue/monsters/place-in-dim monster-types
                                                dimensions))
         (objects '()))
    (list room-number dimensions doors monsters
          objects)))

(defun rogue/room/number (room)
  "The number of ROOM."
  (nth 0 room))

(defun rogue/room/dims (room)
  "The dimensions of ROOM."
  (nth 1 room))

(defun rogue/room/center (room)
  "The center position of ROOM."
  (rogue/dim/center (rogue/room/dims room)))

(defun rogue/room/doors (room)
  "The list of doors from ROOM."
  (nth 2 room))

(defun rogue/room/monsters (room)
  "The list of monsters from ROOM."
  (seq-filter #'rogue/monster/alive-p
              (nth 3 room)))

(defun rogue/room/objects (room)
  "The list of objects from ROOM."
  (nth 4 room))

;;; Doors ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +rogue-door-placements+ '(NORTH EAST SOUTH WEST))

(defun rogue/doors/place (target-rooms)
  "Place doors leading to TARGET-ROOMS at the different possible placements.

At most one door is placed on each of the four walls."
  (if (> (length target-rooms) 4)
      (error "Too many target-rooms: %s" target-rooms)
    ;; Add the target-rooms in random locations.
    (let ((placements (rogue/util/shuffle-list +rogue-door-placements+)))
      (seq-mapn #'rogue/door/make placements target-rooms))))

(defun rogue/door/make (placement target-room)
  "Make a door with given PLACEMENT leading to TARGET-ROOM."
  (cons placement target-room))

(defun rogue/door/placement (door)
  "The placement of DOOR."
  (car door))

(defun rogue/door/target (door)
  "The target of DOOR."
  (cdr door))

;;; Monsters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/monsters/roll-position (dimensions occupied)
  "Find a random position within DIMENSIONS outside the OCCUPIED spaces."
  ;; Don't place monsters on wall tiles.
  (let* ((x (+ 2 (random (- (rogue/dim/x dimensions) 4))))
         (y (+ 2 (random (- (rogue/dim/y dimensions) 4))))
         (pos (rogue/pos x y)))
    (if (or (seq-contains occupied pos)
            ;; Center tiles of rooms are reserved for other entities.
            (equal pos (rogue/dim/center dimensions)))
        (rogue/monsters/roll-position dimensions occupied)
      pos)))

(defun rogue/monsters/place-in-dim (monster-types dimensions)
  "Place monsters of the given MONSTER-TYPES in a space of DIMENSIONS."
  (let ((monsters (mapcar #'rogue/monsters/make monster-types))
        (occupied '()))
    (dolist (monster monsters)
      (let ((position (rogue/monsters/roll-position dimensions occupied)))
        (push position occupied)
        (rogue/monster/set-position monster position)))
    monsters))

(defun rogue/monsters/monster-at (pos monsters)
  "Find the monster at POS in the list of MONSTERS."
  (seq-find (lambda (m)
              (equal pos (rogue/monster/pos m)))
            monsters
            nil))

(defun rogue/monsters/move ()
  "Make monsters move towards the player."
  (let ((monsters-after-move '()))
    (dolist (m (rogue/room/monsters *rogue-current-room*))
      (let* ((m-pos (rogue/monster/pos m))
             (x-diff (- (rogue/pos/x *rogue-player-position*)
                        (rogue/pos/x (rogue/monster/pos m))))
             (y-diff (- (rogue/pos/y *rogue-player-position*)
                        (rogue/pos/y (rogue/monster/pos m))))
             (new-pos
              (if (> (abs x-diff) (abs y-diff))
                  (rogue/pos/add m-pos (rogue/pos (rogue/util/sign x-diff) 0))
                (rogue/pos/add m-pos (cons 0 (rogue/util/sign y-diff))))))
        (unless (rogue/monsters/monster-at new-pos monsters-after-move)
          (rogue/monster/set-position m new-pos)
          (push m monsters-after-move))))))

(defun rogue/monsters/make (type)
  "Create a new monster of type TYPE.

A monster is represented by a structure as follows:
'(TYPE SYMBOL (CURRENT-HP . MAX-HP) DAMAGE POSITION)."
  (cond
   ((eq type 'OGRE)
    (list type "O" (cons 5 5) 1 nil))
   ((eq type 'SKELETON)
    (list type "S" (cons 3 3) 1 nil))
   ((eq type 'GOAT)
    (list type "G" (cons 4 4) 2 nil))
   ((eq type 'CENTAUR)
    (list type "C" (cons 11 11) 3 nil))
   ((eq type 'MEDUSA)
    (list type "H" (cons 12 12) 4 nil))
   ((eq type 'DRAGON)
    (list type "D" (cons 18 18) 6 nil))
   (t (error "Unknown monster type '%s'" type))))

(defun rogue/monster/type (monster)
  "The type of MONSTER."
  (nth 0 monster))

(defun rogue/monster/symbol (monster)
  "The symbol representing MONSTER in the dungeon."
  (nth 1 monster))

(defun rogue/monster/hp (monster)
  "The current hitpoints of MONSTER."
  (car (nth 2 monster)))

(defun rogue/monster/max-hp (monster)
  "The maximal hitpoints of MONSTER."
  (cdr (nth 2 monster)))

(defun rogue/monster/damage (monster)
  "The damage dealt by MONSTER."
  (nth 3 monster))

(defun rogue/monster/pos (monster)
  "The current position of MONSTER."
  (nth 4 monster))

(defun rogue/monster/set-position (monster position)
  "Set the MONSTER's location to POSITION."
  (setcar (nthcdr 4 monster) position))

(defun rogue/monster/alive-p (monster)
  "Whether MONSTER is alive."
  (< 0 (rogue/monster/hp monster)))

(defun rogue/monster/reduce-hp (monster damage)
  "Attack MONSTER with an attack dealing DAMAGE."
  (let ((current-hp (rogue/monster/hp monster)))
    (setcar (nth 2 monster)
            (- current-hp damage))))

;;; Game-Specific Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/function/healer (amount)
  "Create a function that heals the player for AMOUNT when executed."
  (lambda ()
    (setq *rogue-player-current-hp*
          (min *rogue-player-max-hp*
               (+ *rogue-player-current-hp* amount)))
    (message (format "You have been healed for %d points" amount))))

(defun rogue/function/not-in-combat ()
  "Function for an action unavailable in a fight."
  (message "Not available when in combat."))

(defun rogue/function/only-in-combat ()
  "Function for an action only available in a fight."
  (message "Only available in a fight."))

(defun rogue/function/afflict-damage (amount)
  "Afflict AMOUNT damage to the current monster, if any."
  (unless *rogue-current-monster*
    (message "No monster present to deal damage to."))
  (rogue/monster/reduce-hp *rogue-current-monster* amount))

;;; Items ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/weapon/make (type)
  "Create a weapon of type TYPE."
  (cond ((null type)
         (list 'FIST 1 1))
        ((eq type 'SWORD)
         (list type 2 4))
        (t (error "Unknown weapon type '%s'" type))))

(defun rogue/weapon/type (weapon)
  "The type of WEAPON."
  (car weapon))

(defun rogue/weapon/dmg (weapon)
  "The damage for the next attack with WEAPON."
  (+ (rogue/weapon/min-dmg weapon)
     (random (+ 1 (rogue/weapon/max-dmg weapon)))))

(defun rogue/weapon/min-dmg (weapon)
  "The minimal damage of WEAPON."
  (cadr weapon))

(defun rogue/weapon/max-dmg (weapon)
  "The maximal damage of WEAPON."
  (caddr weapon))

;;; Spells ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/spell/make (name in-dungeon in-combat)
  "Create spell NAME with actions to take when IN-DUNGEON or IN-COMBAT.

The latter two arguments need to be callable functions without arguments."
  (list name in-dungeon in-combat))

(defun rogue/spell/name (spell)
  "The name of SPELL."
  (car spell))

(defun rogue/spell/cast-in-dungeon (spell)
  "Cast SPELL while exploring the dungeon."
  (funcall (cadr spell)))

(defun rogue/spell/cast-in-combat (spell)
  "Cast SPELL while in a fight."
  (funcall (caddr spell)))

(defun rogue/spells/init ()
  (let ((heal-3 (rogue/function/healer 3)))
    (setq *rogue/spell/heal* (rogue/spell/make 'HEAL heal-3 heal-3))))

;;; Positioning ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/pos (x y)
  "The position at X, Y."
  (cons x y))

(defun rogue/pos/x (p)
  "The X component of position P."
  (car p))

(defun rogue/pos/y (p)
  "The Y component of position P."
  (cdr p))

(defun rogue/pos/add (pos-a pos-b)
  "Add coordinates of position POS-A and POS-B."
  (rogue/pos (+ (rogue/pos/x pos-a) (rogue/pos/x pos-b))
             (+ (rogue/pos/y pos-a) (rogue/pos/y pos-b))))

(defun rogue/pos/within-one (pos-a pos-b)
  "Whether two positions are in immediate vertical or horizontal proximity"
  (let ((x-diff (rogue/util/abs (- (rogue/pos/x pos-a) (rogue/pos/x pos-b))))
        (y-diff (rogue/util/abs (- (rogue/pos/y pos-a) (rogue/pos/y pos-b)))))
    (or (and (= x-diff 0) (<= y-diff 1))
        (and (= y-diff 0) (<= x-diff 1)))))

(defun rogue/pos/x-inc (p)
  "Increase the X component of position P."
  (let ((x (rogue/pos/x p)))
    (setcar p
            (1+ x))))

(defun rogue/pos/x-dec (p)
  "Decrease the X component of position P."
  (let ((x (rogue/pos/x p)))
    (setcar p
            (1- x))))

(defun rogue/pos/y-inc (p)
  "Increase the X component of position P."
  (let ((y (rogue/pos/y p)))
    (setcdr p
            (1+ y))))

(defun rogue/pos/y-dec (p)
  "Decrease the X component of position P."
  (let ((y (rogue/pos/y p)))
    (setcdr p
            (1- y))))

(defun rogue/dim (x y)
  "A dimension object of size X, Y."
  (cons x y))

(defun rogue/dim/x (d)
  "The X dimension of D."
  (car d))

(defun rogue/dim/y (d)
  "The Y dimension of D."
  (cdr d))

(defun rogue/dim/center (dim)
  "The center of the area described by DIM."
  (rogue/pos (/ (rogue/dim/x dim)
                2)
             (/ (rogue/dim/y dim)
                2)))

;;; General Utilities ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/util/shuffle-list (sequence)
  "A list with the same elements as SEQUENCE but in randomized order."
  ;; Ordering is achieved by attaching a random number to each element which
  ;; is used for sorting. The attachment is then removed again.
  ;; Note that, since `sort' is stable, this is no perfect randomization as
  ;; the initial ordering can `bleed through' with equal random attachments.
  (let ((attach-random (lambda (x)
                         (list x
                               (random))))
        (asc-by-second (lambda (a b)
                         (< (cadr a) (cadr b)))))
    (mapcar #'car
            (sort (mapcar attach-random sequence)
                  asc-by-second))))

(defun rogue/util/sublists (min-len len sequence)
  "Partition SEQUENCE into sublists of nominal length LEN, at least MIN-LEN."
  (if (> min-len (length sequence))
      '()
    (cons (seq-take sequence len) (rogue/util/sublists min-len
                                                       len
                                                       (cdr sequence)))))

(defun rogue/util/range (lower-limit upper-limit)
  "The numbers from LOWER-LIMIT up to, but not including, UPPER-LIMIT."
  (cond
   ((= lower-limit upper-limit) '())
   ((> lower-limit upper-limit)
    (error "Invalid range: %d to %d" lower-limit
           upper-limit))
   (t (cons lower-limit (rogue/util/range (+ lower-limit 1)
                                          upper-limit)))))

(defun rogue/util/any-null-p (sequences)
  "T if any of SEQUENCES is empty, else nil."
  (cond
   ((null sequences) nil)
   ((null (car sequences)) t)
   (t (rogue/util/any-null-p (cdr sequences)))))

(defun rogue/util/abs (number)
  "The absolute magnitude of a number."
  (if (< number 0)
      (- number)
    number))

(defun rogue/util/sign (number)
  "The sign of NUMBER.

-1 if NUMBER is negative, +1 if it is positive, and 0 if it is itself zero."
  (cond ((> number 0) 1)
        ((< number 0) -1)
        (t 0)))

;;; rogue.el ends here
