;;; rogue.el --- Simple roguelike within Emacs. -*- lexical-binding: t; -*-

;;; Commentary:
;; Not participating in 7drl, but it inspired me to create something, anyway.

;;; License: GNU GPLv3

;;; Code:

;; Some parts of the code could conceivably benefit from proper OO programming,
;; namely using eieio.el. However, it was skipped for a more familiar
;; list-based approach which allows for sufficient abstractions.
;;
;; Also, while it may not seem wise, an attempt is made to steer clear of
;; any cl.el features, raising the need to implement a few basic utilities.
;; This is not a sign of disdain for Common Lisp, rather than a choice to
;; explore the natural capabilities of Emacs Lisp.

(require 'seq)

;;; Display ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst +rogue-player-symbol+ "@"
  "Representation of the player on the board.")
(defconst +rogue-buffer-name+ "A simple roguelike"
  "The name of the game buffer.")

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

;;; Game State ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Player related

(defvar *rogue-player-max-health* 0
  "The maximum hitpoints of the player.")
(defvar *rogue-player-current-health* 0
  "The player's current hitpoints.")

(defvar *rogue-player-max-mana* 0
  "The maximum mana points of the player.")
(defvar *rogue-player-current-mana* 0
  "The player's current mana points.")

(defvar *rogue-message* nil
  "The latest message for the player.")
(defvar *rogue-player-position* nil
  "The position of the player within the current room.")
(defvar *rogue-player-armor* nil
  "The currently equipped items.")
(defvar *rogue-player-inventory* nil
  "The player's inventory.")
(defvar *rogue-player-weapon* nil
  "The currently held weapon.")
(defvar *rogue-inventory-selection* 0
  "The position of the currently selected inventory item.")
(defvar *rogue-player-spell* nil
  "The currently active spell.")
(defvar *rogue-player-available-spells* nil
  "The spells available to the player.")
(defvar *rogue-spell-selection* 0
  "The position of the currently selected spell.")

(make-variable-buffer-local '*rogue-message*)
(make-variable-buffer-local '*rogue-player-max-health*)
(make-variable-buffer-local '*rogue-player-position*)
(make-variable-buffer-local '*rogue-player-armor*)
(make-variable-buffer-local '*rogue-player-inventory*)
(make-variable-buffer-local '*rogue-player-weapon*)
(make-variable-buffer-local '*rogue-inventory-selection*)
(make-variable-buffer-local '*rogue-player-spell*)
(make-variable-buffer-local '*rogue-player-available-spells*)
(make-variable-buffer-local '*rogue-spell-selection*)

;; Level related

(defconst +rogue-rooms-per-level+ 5 ;25
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

;;; Keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconst rogue/dungeon-keymap
  (let ((map (make-sparse-keymap 'rogue/dungeon-keymap)))
    (define-key map [left] #'rogue/player/move-left)
    (define-key map [right] #'rogue/player/move-right)
    (define-key map [up] #'rogue/player/move-up)
    (define-key map [down] #'rogue/player/move-down)
    (define-key map "i" #'rogue/player/access-inventory)
    (define-key map "m" #'rogue/player/access-magic-menu)
    (define-key map "s" #'rogue/player/cast-spell)
    (define-key map "q" #'rogue/quit)
    map))

(defconst +rogue/dungeon/usage+
  (concat "[up/down/left/right] move -- [s] cast current spell\n"
          "[i] access inventory -- [m] access magic menu\n"
          "[q] quit the game\n"))

(defconst rogue/fight-keymap
  (let ((map (make-sparse-keymap 'rogue/fight-keymap)))
    (define-key map "a" #'rogue/fight/weapon-attack)
    (define-key map "s" #'rogue/fight/cast-spell)
    (define-key map "q" #'rogue/fight/exit)
    (define-key map [up] #'rogue/fight/cannot-move)
    (define-key map [right] #'rogue/fight/cannot-move)
    (define-key map [down] #'rogue/fight/cannot-move)
    (define-key map [left] #'rogue/fight/cannot-move)
    map))

(defconst +rogue/fight/usage+
  "[a] attack with weapon -- [s] cast current spell -- [q] exit the fight\n")

(defconst rogue/inventory-keymap
  (let ((map (make-sparse-keymap 'rogue/inventory-keymap)))
    (define-key map "e" #'rogue/inventory/equip-toggle)
    (define-key map "u" #'rogue/inventory/use-item)
    (define-key map "d" #'rogue/inventory/drop-item)
    (define-key map "q" #'rogue/inventory/exit)
    (define-key map [down] #'rogue/inventory/select-next)
    (define-key map [up] #'rogue/inventory/select-previous)
    map))

(defconst +rogue/inventory/usage+
  (concat "[up/down] change selection -- [e] toggle equip -- [u] consume item\n"
          "[d] drop item -- [q] exit inventory\n"))

(defconst rogue/magic-menu-keymap
  (let ((map (make-sparse-keymap 'rogue/magic-menu-keymap)))
    (define-key map "e" #'rogue/magic-menu/activate-spell)
    (define-key map "q" #'rogue/magic-menu/exit)
    (define-key map [down] #'rogue/magic-menu/select-next)
    (define-key map [up] #'rogue/magic-menu/select-previous)
    map))

(defconst +rogue/magic-menu/usage+
  "[up/down] change selection -- [e] set spell active -- [q] exit magic menu\n")

;;; General ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (setq *rogue-player-max-health* 10)
  (setq *rogue-player-current-health* *rogue-player-max-health*)
  (setq *rogue-player-max-mana* 10)
  (setq *rogue-player-current-mana* *rogue-player-max-mana*)
  (setq *rogue-message* "")
  (setq *rogue-player-weapon* (rogue/weapon/get 'SWORD))
  (setq *rogue-player-armor* (list (rogue/armor/get 'SHIELD)))
  (setq *rogue-player-inventory*
        `(,*rogue-player-weapon*
          ,(rogue/consumable/get 'HEALTH-POTION)
          ,(rogue/consumable/get 'MANA-POTION)
          ,@*rogue-player-armor*))
  (setq *rogue-player-spell* (rogue/spell/get 'HEAL))
  (setq *rogue-player-available-spells*
        (list *rogue-player-spell* (rogue/spell/get 'LIGHTNING)))
  (setq *rogue-current-monster* nil)
  (setq *rogue-fight-log* nil)
  (rogue/draw/dungeon))

(defun rogue/quit ()
  "Quit the game of Rogue."
  (interactive)
  (when (yes-or-no-p "Do you really wish to quit Rogue?")
    (kill-buffer +rogue-buffer-name+)))

(defun rogue/player/move-done ()
  "The function to call after the player made their move."
  (rogue/monsters/move)
  (cond ((rogue/player/monster-collision-p) (rogue/fight/start))
        ((rogue/player/object-collision-p)
         (let ((object (rogue/player/object-collision-p)))
           (rogue/objects/interact object)))
        (t (rogue/draw/dungeon))))

(defun rogue/back-to-dungeon ()
  "Go back to dungeon view."
  (use-local-map rogue/dungeon-keymap)
  (rogue/draw/dungeon))

;;; Graphics ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro rogue/draw/with-writable-buffer (&rest body)
  "Execute BODY within a writable buffer."
  `(progn (setq-local buffer-read-only nil)
          (erase-buffer)
          ,@body
          (setq-local buffer-read-only t)))

(defun rogue/draw/dungeon ()
  "Draw the dungeon screen."
  (rogue/draw/with-writable-buffer
   (rogue/draw/stat-header)
   (let* ((dims (rogue/room/dims *rogue-current-room*))
          (monsters (rogue/room/monsters *rogue-current-room*))
          (objects (rogue/room/objects *rogue-current-room*))
          (door-places (mapcar #'rogue/door/placement
                               (rogue/room/doors *rogue-current-room*)))
          (top-pad (/ (- (rogue/dim/y dims)
                         +rogue-room-max-side-length+)
                      2)))
     (dotimes (_ top-pad)
       (newline))
     (dotimes (n (rogue/dim/y dims))
       (rogue/draw/dungeon-row n dims monsters objects door-places)))
   (newline)
   (insert *rogue-message*)
   (newline)
   (insert +rogue/dungeon/usage+)))

(defun rogue/draw/inventory ()
  "Draw the inventory access screen."
  (rogue/draw/with-writable-buffer
   (rogue/draw/stat-header)
   (let ((counter 0))
     (dolist (item *rogue-player-inventory*)
       (if (= counter *rogue-inventory-selection*)
           (insert " * ")
         (insert "   "))
       (if (rogue/item/equipped-p item)
           (insert "[+] ")
         (insert "[ ] "))
       (insert (format "%s\n" (rogue/item/name item)))
       (setq counter (1+ counter))))
   (newline)
   (insert *rogue-message*)
   (newline)
   (insert +rogue/inventory/usage+)))

(defun rogue/draw/magic-menu ()
  "Draw the magic menu screen."
  (rogue/draw/with-writable-buffer
   (rogue/draw/stat-header)
   (let ((counter 0))
     (dolist (spell *rogue-player-available-spells*)
       (if (= counter *rogue-spell-selection*)
           (insert " * ")
         (insert "   "))
       (if (rogue/spell/active-p spell)
           (insert "[+] ")
         (insert "[ ] "))
       (insert (format "%s\n" (rogue/spell/name spell)))
       (setq counter (1+ counter))))
   (newline)
   (insert +rogue/magic-menu/usage+)))

(defun rogue/draw/fight ()
  "Draw the fight screen for fighting the monster at the current position."
  (rogue/draw/with-writable-buffer
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
   (newline)
   (insert +rogue/fight/usage+)))

(defun rogue/draw/stat-header ()
  "Print a general stat header."
  (insert (format "Level:  %d\n"
                  (rogue/level/number *rogue-current-level*)))
  (insert (format "Room:   %d\n"
                  (rogue/room/number *rogue-current-room*)))
  (insert (format "Health: %d/%d\n"
                  *rogue-player-current-health*
                  *rogue-player-max-health*))
  (insert (format "Mana:   %d/%d\n"
                  *rogue-player-current-mana*
                  *rogue-player-max-mana*))
  (newline)
  (insert (format "Weapon: %s\n"
                  (rogue/item/name *rogue-player-weapon*)))
  (insert "Armor: ")
  (dolist (armor *rogue-player-armor*)
    (insert (format " %s" (rogue/item/name armor))))
  (insert "\n")
  (insert (format "Spell:  %s\n" (rogue/spell/name *rogue-player-spell*)))
  (insert "\n"))

(defun rogue/draw/dungeon-row (n room-dims monsters objects door-places)
  "Draw row N of a ROOM-DIMS size with MONSTERS, OBJECTS, and DOOR-PLACES."
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
      (rogue/draw/dungeon-interior n monsters objects)
      (insert (if has-east-door +rogue/door-top+ +rogue/vertic-wall+)))
     ((= n horiz-door-line)
      (insert (if has-west-door +rogue/empty-tile+ +rogue/vertic-wall+))
      (rogue/draw/dungeon-interior n monsters objects)
      (insert (if has-east-door +rogue/empty-tile+ +rogue/vertic-wall+)))
     ((= n (1+ horiz-door-line))
      (insert (if has-west-door +rogue/door-bottom+ +rogue/vertic-wall+))
      (rogue/draw/dungeon-interior n monsters objects)
      (insert (if has-east-door +rogue/door-bottom+ +rogue/vertic-wall+)))
     ;; Normal segment, no doors.
     (t (insert +rogue/vertic-wall+)
        (rogue/draw/dungeon-interior n monsters objects)
        (insert +rogue/vertic-wall+)))
    (newline)))

(defun rogue/draw/dungeon-interior (n monsters objects)
  "Draw line N of the inside of the current room.

Place the player as well as MONSTERS and OBJECTS where appropriate."
  (let ((y n)
        (room-dims (rogue/room/dims *rogue-current-room*)))
    (dotimes (k (- (rogue/dim/x room-dims)
                   2))
      (let* ((x (+ k 1))
             (pos (rogue/pos x y))
             (m (rogue/monsters/monster-at pos monsters))
             (o (rogue/objects/object-at pos objects)))
        (cond
         ((equal pos *rogue-player-position*)
          (insert +rogue-player-symbol+))
         (m (insert (rogue/monster/symbol m)))
         (o (insert (rogue/object/symbol o)))
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

;;; Player ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/player/reduce-hp (amount)
  "Reduce the player's hitpoints by AMOUNT."
  (setq *rogue-player-current-health*
        (- *rogue-player-current-health* amount)))

(defun rogue/player/alive-p ()
  "Whether the player is currently alive."
  (> *rogue-player-current-health* 0))

;;; Movement ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun rogue/player/make-move (modifier-function)
  "Attempt to make a move based on the MODIFIER-FUNCTION."
  (let ((old-pos (rogue/pos (rogue/pos/x *rogue-player-position*)
                            (rogue/pos/y *rogue-player-position*))))
    (funcall modifier-function *rogue-player-position*)
    (cond
     ((rogue/player/wall-collision-p)
      (setq *rogue-player-position* old-pos)
      (rogue/message/set "Ouch!")
      (rogue/draw/dungeon))
     (t (rogue/message/clear)
        (rogue/player/check-collisions)))))

(defun rogue/player/move-left ()
  "Move the player to the left."
  (interactive)
  (rogue/player/make-move #'rogue/pos/x-dec))

(defun rogue/player/move-right ()
  "Move the player to the right."
  (interactive)
  (rogue/player/make-move #'rogue/pos/x-inc))

(defun rogue/player/move-up ()
  "Move the player up."
  (interactive)
  (rogue/player/make-move #'rogue/pos/y-dec))

(defun rogue/player/move-down ()
  "Move the player down."
  (interactive)
  (rogue/player/make-move #'rogue/pos/y-inc))

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
  "Whether the player collided with any monsters."
  (let* ((monsters (rogue/room/monsters *rogue-current-room*))
         (at-player-pos
          (seq-filter
           (lambda (monster)
             (rogue/pos/within-one (rogue/monster/pos monster)
                                   *rogue-player-position*))
           monsters)))
    (unless (null at-player-pos)
      (car at-player-pos))))

(defun rogue/player/object-collision-p ()
  "Whether the player collided with an object."
  (let ((objects (rogue/room/objects *rogue-current-room*)))
    (rogue/objects/object-at *rogue-player-position* objects)))

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

;;; Inventory ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/player/access-inventory ()
  "Access the player's inventory."
  (interactive)
  (use-local-map rogue/inventory-keymap)
  (setq *rogue-inventory-selection* 0)
  (rogue/message/clear)
  (rogue/draw/inventory))

(defun rogue/inventory/exit ()
  "Exit the inventory screen."
  (interactive)
  (rogue/message/clear)
  (rogue/back-to-dungeon))

(defun rogue/inventory/selected-item ()
  "The currently selected item."
  (nth *rogue-inventory-selection* *rogue-player-inventory*))

(defun rogue/inventory/add-item (item)
  "Add ITEM to the inventory."
  (push item *rogue-player-inventory*))

(defun rogue/inventory/discard-selected-item ()
  "Discard ITEM from the inventory."
  (setq *rogue-player-inventory*
        (append (seq-take *rogue-player-inventory*
                          *rogue-inventory-selection*)
                (seq-drop *rogue-player-inventory*
                          (1+ *rogue-inventory-selection*))))
  (unless (= *rogue-inventory-selection* 0)
    (setq *rogue-inventory-selection*
          (1- *rogue-inventory-selection*))))

(defun rogue/inventory/use-item ()
  "Equip the currently selected item if possible."
  (interactive)
  (let ((item (rogue/inventory/selected-item)))
    (rogue/item/consume item))
  (rogue/draw/inventory))

(defun rogue/inventory/equip-toggle ()
  "Equip the currently selected item if possible."
  (interactive)
  (let ((item (nth *rogue-inventory-selection* *rogue-player-inventory*)))
    (if (rogue/item/equipped-p item)
        (rogue/item/unequip item)
      (rogue/item/equip item)))
  (rogue/draw/inventory))

(defun rogue/inventory/drop-item ()
  "Drop the currently selected item.

Equipped items will have to be unequipped first."
  (interactive)
  (let ((item (rogue/inventory/selected-item)))
    (if (rogue/item/equipped-p item)
        (error "Cannot drop %s, need to unequid first"
               (rogue/item/name item))
      (rogue/inventory/discard-selected-item)))
  (rogue/draw/inventory))

(defun rogue/inventory/select-next ()
  "Select the next inventory item."
  (interactive)
  (setq *rogue-inventory-selection*
        (min (1+ *rogue-inventory-selection*)
             (1- (length *rogue-player-inventory*))))
  (rogue/draw/inventory))

(defun rogue/inventory/select-previous ()
  "Select the previous inventory item."
  (interactive)
  (setq *rogue-inventory-selection*
        (max (1- *rogue-inventory-selection*)
             0))
  (rogue/draw/inventory))

;;; Magic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/player/cast-spell ()
  "Cast the currently active spell."
  (interactive)
  (if *rogue-player-spell*
      (progn
        (rogue/spell/cast-in-dungeon *rogue-player-spell*)
        (rogue/draw/dungeon))
    (error "No spell selected")))

(defun rogue/player/access-magic-menu ()
  "Access the magic menu."
  (interactive)
  (use-local-map rogue/magic-menu-keymap)
  (rogue/draw/magic-menu))

(defun rogue/magic-menu/exit ()
  "Return to dungeon view."
  (interactive)
  (use-local-map rogue/dungeon-keymap)
  (rogue/draw/dungeon))

(defun rogue/magic-menu/select-next ()
  "Select the next spell in the menu."
  (interactive)
  (setq *rogue-spell-selection*
        (min (1+ *rogue-spell-selection*)
             (1- (length *rogue-player-available-spells*))))
  (rogue/draw/magic-menu))

(defun rogue/magic-menu/select-previous()
  "Select the previous spell in the menu."
  (interactive)
  (setq *rogue-spell-selection*
        (max (1- *rogue-spell-selection*)
             0))
  (rogue/draw/magic-menu))

(defun rogue/magic-menu/activate-spell ()
  "Activate the currently selected spell."
  (interactive)
  (let ((spell (nth *rogue-spell-selection* *rogue-player-available-spells*)))
    (setq *rogue-player-spell* spell))
  (rogue/draw/magic-menu))

;;; Fight ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/fight/start ()
  "Enter fight mode."
  (let ((monster (rogue/player/monster-collision-p)))
    (unless monster (error "Call to FIGHT with no monster present"))
    (setq *rogue-current-monster* monster)
    (setq *rogue-fight-log* nil)
    (rogue/fight/add-to-log "You are now fighting %s"
                            (rogue/monster/type monster)))
  (use-local-map rogue/fight-keymap)
  (rogue/draw/fight)
  (message "Started fighting"))

(defun rogue/fight/exit ()
  "Exit fight mode."
  (interactive)
  (cond ((not (rogue/player/alive-p))
         (rogue/quit))
        ((rogue/monster/alive-p *rogue-current-monster*)
         (error "Cannot leave an ongoing fight"))
        (t
         (rogue/message/set "You defeated %s"
                            (rogue/monster/type *rogue-current-monster*))
         (setq *rogue-current-monster* nil)
         (rogue/back-to-dungeon))))

(defun rogue/fight/add-to-log (string &rest args)
  "Add a message to the current fight log.

Format according to the format STRING using ARGS."
  (unless *rogue-current-monster*
    (error "Not in a fight but attempting to extend fight log"))
  (push (apply #'format string args) *rogue-fight-log*))

(defun rogue/fight/cannot-move ()
  "Signal the player that moving is disabled during a fight."
  (interactive)
  (rogue/notify "Cannot move -- Fighting"))

(defun rogue/fight/move-done ()
  "Evaluations after attacking a monster."
  (if (rogue/monster/alive-p *rogue-current-monster*)
     (rogue/fight/monster-attack)
    (rogue/fight/loot)))

(defun rogue/fight/loot ()
  "Look for loot among the remains of a slain monster."
  (let* ((roll (random 10))
         (item (cond ((< roll 1)
                      (apply #'rogue/weapon/make
                             (seq-random-elt +rogue-all-weapons+)))
                     ((< roll 2)
                      (apply #'rogue/armor/make
                             (seq-random-elt +rogue-all-armor+)))
                     ((< roll 5)
                      (apply #'rogue/consumable/make
                             (seq-random-elt +rogue-all-consumables+)))
                     (t nil))))
    (when item
      (rogue/inventory/add-item item)
      (rogue/fight/add-to-log "You found %s on the monster's corpse."
                              (rogue/item/name item))))
  (rogue/draw/fight)
  (rogue/notify "Press q to exit the fight screen"))

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
                (rogue/fight/add-to-log
                 "You hit %s with %s for %d damage."
                 mtype
                 (rogue/item/name *rogue-player-weapon*)
                 dmg)
                (when (not (rogue/monster/alive-p *rogue-current-monster*))
                  (rogue/fight/add-to-log "You killed %s." mtype)))
              (rogue/draw/fight)
              (rogue/fight/move-done))
          (rogue/notify "The monster is already dead"))
      (error "Not in a fight"))))

(defun rogue/fight/cast-spell ()
  "Attack with the currently active spell."
  (interactive)
  (when (rogue/player/alive-p)
    (if *rogue-current-monster*
        (if *rogue-player-spell*
            (progn
              (rogue/spell/cast-in-combat *rogue-player-spell*)
              (rogue/draw/fight)
              (rogue/fight/move-done))
          (message "No spell selected"))
      (error "Not in a fight"))))

(defun rogue/fight/monster-attack ()
  "The current monster attacks the player."
  (let ((dmg (rogue/monster/damage *rogue-current-monster*)))
    (dolist (armor *rogue-player-armor*)
      (setq dmg (rogue/armor/take-damage armor dmg)))
    (rogue/player/reduce-hp dmg)
    (rogue/fight/add-to-log "%s hit you for %d damage."
                            (rogue/monster/type *rogue-current-monster*)
                            dmg)
    (if (not (rogue/player/alive-p))
        (progn
          (rogue/fight/add-to-log "You have died.")
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
    (let* ((first-level (assoc 1 levels))
           (last-room (rogue/level/last-room first-level)))
      (rogue/room/place-object-center last-room (rogue/stairs/make 1 2)))
    (dolist (lvl-num (rogue/util/range 2 +rogue-num-levels+))
      (let ((level (assoc lvl-num levels)))
        (rogue/room/place-object-center
         (rogue/level/first-room level)
         (rogue/stairs/make lvl-num (1- lvl-num)))
        (rogue/room/place-object-center
         (rogue/level/last-room level)
         (rogue/stairs/make lvl-num (1+ lvl-num)))))
    levels))

(defun rogue/levels/make-one (level-number)
  "Create and populate level LEVEL-NUMBER."
  (let* ((room-numbers
          (mapcar (lambda (x) (+ x (* 100 level-number)))
                  (rogue/util/range 1 (1+ +rogue-rooms-per-level+))))
         (shuffled (rogue/util/shuffle-list room-numbers))
         (partitioned (rogue/util/sublists 2 3 shuffled))
         (rooms-unsorted
          (cons (rogue/room/make
                 (caar partitioned)
                 (list (cadar partitioned)))
                (mapcar (lambda (numbers)
                          (rogue/room/make
                           (cadr numbers)
                           (cons (car numbers) (cddr numbers))))
                        partitioned)))
         (rooms (sort rooms-unsorted
                      (lambda (x y)
                        (< (car x) (car y))))))
    (dolist (room rooms)
      (rogue/room/place-monsters room (rogue/monsters/for-level level-number)))
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
    (let ((room (rogue/level/room level room-number)))
      (when (null room)
        (error "No such room: %d" room-number))
      room)))

;;; Rooms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/roll-monster-position (dimensions occupied)
  "Find a random position within DIMENSIONS outside the OCCUPIED spaces."
  ;; Don't place monsters on wall tiles.
  (let* ((x (+ 2 (random (- (rogue/dim/x dimensions) 4))))
         (y (+ 2 (random (- (rogue/dim/y dimensions) 4))))
         (pos (rogue/pos x y)))
    (if (or (seq-contains occupied pos)
            ;; Center tiles of rooms are reserved for other entities.
            (equal pos (rogue/dim/center dimensions)))
        (rogue/roll-monster-position dimensions occupied)
      pos)))

(defun rogue/rooms/side-len ()
  "One random side length of a room.

Possible sizes are delimited by +ROGUE-MIN-SIDE-LENGTH+ and
+ROGUE-MAX-SIDE-LENGTH+. Always returns an odd number for easier symmetry."
  (let ((addend
         (/ (- +rogue-room-max-side-length+ +rogue-room-min-side-length+) 2)))
    (+ (* 2 (/ +rogue-room-min-side-length+ 2)) (* 2 (random addend)) 1)))

(defun rogue/room/make (room-number adjacent-rooms)
  "Create room ROOM-NUMBER in LEVEL with and doors leading to ADJACENT-ROOMS.

Dimensions, monsters, and objects are chosen randomly in accordance with the
relevant variables."
  (let* ((dimensions (rogue/dim (rogue/rooms/side-len)
                                (rogue/rooms/side-len)))
         (doors (rogue/doors/place adjacent-rooms))
         (monsters '())
         (objects '()))
    (list room-number dimensions doors monsters objects)))

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

(defun rogue/room/occupied-places (room)
  "Places in ROOM that are occupied by monsters or objects."
  ;; The center is always considered occupied.
  (cons (rogue/room/center room)
        (append (mapcar #'rogue/object/pos (rogue/room/objects room))
                (mapcar #'rogue/monster/pos (rogue/room/monsters room)))))

(defun rogue/room/place-monsters (room monster-types)
  "Place monsters of MONSTER-TYPES in ROOM."
  (let ((dimensions (rogue/room/dims room))
        (monsters (mapcar #'rogue/monsters/make monster-types))
        (occupied (list (rogue/room/center room))))
    (dolist (monster monsters)
      (let ((position (rogue/roll-monster-position dimensions occupied)))
        (push position occupied)
        (rogue/monster/set-position monster position)))
    (setcar (nthcdr 3 room) monsters)))

(defun rogue/room/place-object-center (room object)
  "Add OBJECT to ROOM."
  (rogue/object/place-at object (rogue/room/center room))
  (setcar (nthcdr 4 room)
          (cons object (rogue/room/objects room))))

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

;;; Objects ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/objects/object-at (pos objects)
  "Find, among OBJECTS, the object at POS, if any."
  (seq-find (lambda (m)
              (equal pos (rogue/object/pos m)))
            objects
            nil))

(defun rogue/objects/interact (object)
  "Interact with OBJECT."
  (cond ((rogue/object/stairs-p object)
         (when (yes-or-no-p "Use stairs?")
           (rogue/stairs/enter object)))
        (t (error "Don't know how to interact with %S" object))))

(defun rogue/object/make (type position symbol &rest specifics)
  "Create an object of TYPE at POSITION, represented by SYMBOL.

SPECIFICS relating to its type can be given as well."
  `(,type ,position ,symbol ,@specifics))

(defun rogue/object/type (object)
  "The type of OBJECT."
  (car object))

(defun rogue/object/pos (object)
  "The position of OBJECT."
  (cadr object))

(defun rogue/object/symbol (object)
  "The symbol to represent OBJECT."
  (caddr object))

(defun rogue/object/specifics (object)
  "The specifics of OBJECT."
  (cdddr object))

(defun rogue/object/place-at (object position)
  "Place OBJECT at POSITION."
  (setcar (cdr object) position))

(defun rogue/object/stairs-p (object)
  "Whether OBJECT is a staircase."
  (eq (rogue/object/type object) 'STAIRS))

(defun rogue/stairs/make (from-level to-level)
  "Create stairs between FROM-LEVEL and TO-LEVEL."
  (let* ((symbol (if (> from-level to-level)
                     ;; Levels are underground, higher numbers are down.
                     +rogue/stairs-up+
                   +rogue/stairs-down+)))
    (when (= from-level to-level)
      (error "Stairs must connect two distinct levels"))
    (rogue/object/make 'STAIRS '() symbol from-level to-level)))

(defun rogue/stairs/from-level (stairs)
  "The origin level of STAIRS."
  (car (rogue/object/specifics stairs)))

(defun rogue/stairs/to-level (stairs)
  "The target level of STAIRS."
  (cadr (rogue/object/specifics stairs)))

(defun rogue/stairs/up-p (stairs)
  "Whether STAIRS lead up."
  (> (rogue/stairs/from-level stairs)
     (rogue/stairs/to-level stairs)))

(defun rogue/stairs/enter (stairs)
  "Enter STAIRS to get to another level."
  (unless (= (rogue/stairs/from-level stairs)
             (rogue/level/number *rogue-current-level*))
    (error "Not in the right level to enter stairs: %S" stairs))
  (let ((target-level (assoc (rogue/stairs/to-level stairs)
                             *rogue-levels*)))
    (setq *rogue-current-level* target-level)
    (setq *rogue-current-room*
          (if (rogue/stairs/up-p stairs)
              (rogue/level/last-room target-level)
            (rogue/level/first-room target-level)))
    (setq *rogue-player-position*
          (rogue/room/center *rogue-current-room*)))
  (rogue/draw/dungeon))

;;; Monsters ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun rogue/monsters/for-level (level)
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

(defun rogue/message/set (string &rest args)
  "Set the current message for the player, format based on STRING using ARGS."
  (setq *rogue-message* (apply #'format string args)))

(defun rogue/message/clear ()
  "Set the current message for the player, format based on STRING using ARGS."
  (rogue/message/set ""))

(defun rogue/notify (string &rest args)
  "Show a notification for the player, format based on STRING using ARGS."
  (message (apply #'format string args)))

(defun rogue/function/healer (amount)
  "Create a function that heals the player for AMOUNT when called."
  (lambda ()
    (let ((healed
           (min amount
                (- *rogue-player-max-health* *rogue-player-current-health*))))
      (setq *rogue-player-current-health*
            (+ *rogue-player-current-health* healed))
      (format "You regain %d hitpoints" healed))))

(defun rogue/function/manarest (amount)
  "Create a function that restores the player's mana for AMOUNT when called."
  (lambda ()
    (let ((restored
           (min amount
                (- *rogue-player-max-mana* *rogue-player-current-mana*))))
      (setq *rogue-player-current-mana*
            (+ *rogue-player-current-mana* restored))
      (format "You regain %d mana" restored))))

(defun rogue/function/not-in-combat ()
  "Function for an action unavailable in a fight."
  (rogue/notify "Not available when in combat."))

(defun rogue/function/only-in-combat ()
  "Function for an action only available in a fight."
  (rogue/message/set "Only available in a fight."))

(defun rogue/function/damage-dealer (amount)
  "Afflict AMOUNT damage to the current monster, if any."
  (lambda ()
    (unless *rogue-current-monster*
      (message "No monster present to deal damage to."))
    (rogue/monster/reduce-hp *rogue-current-monster* amount)
    (format "You deal %d damage" amount)))

;;; Items ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/item/make (type name &rest specifics)
  "Create an item with the given TYPE and NAME.

TYPE can be one of the symbols WEAPON, ARMOR, and CONSUMABLE.

The remaining SPECIFICS vary based on the chosen type. Checking and handling
them is the responsibility of more specialized functions."
  (apply #'list type name specifics))

(defun rogue/item/type (item)
  "The type of ITEM."
  (car item))

(defun rogue/item/name (item)
  "The name of ITEM."
  (if (null item)
      'NOTHING
    (cadr item)))

(defun rogue/item/weapon-p (item)
  "Whether ITEM is a weapon."
  (eq (rogue/item/type item) 'WEAPON))

(defun rogue/item/armor-p (item)
  "Whether ITEM is a piece of armor."
  (eq (rogue/item/type item) 'ARMOR))

(defun rogue/item/consumable-p (item)
  "whether ITEM is consumable."
  (eq (rogue/item/type item) 'CONSUMABLE))

(defun rogue/item/consume (item)
  "Consume a consumable ITEM."
  (unless (rogue/item/consumable-p item)
    (error "Cannot consume %S" item))
  (rogue/message/set "%s" (funcall (car (rogue/item/specifics item))))
  (rogue/inventory/discard-selected-item)
  (rogue/draw/inventory))

(defun rogue/item/equip (item)
  "Equip ITEM, either as a weapon or a piece of armor."
  (cond ((rogue/item/weapon-p item) (setq *rogue-player-weapon* item))
        ((rogue/item/armor-p item) (push item *rogue-player-armor*))
        (t (error "Cannot equip %S" item))))

(defun rogue/item/unequip (item)
  "Unequip ITEM."
  (cond ((rogue/item/weapon-p item) (setq *rogue-player-weapon* nil))
        ((rogue/item/armor-p item)
         (setq *rogue-player-armor*
               (seq-remove (lambda (it) (eq it item))
                           *rogue-player-armor*)))
        (t (error "Not equippable: %S" item))))

(defun rogue/item/specifics (item)
  "The specific (type-dependent) properties of ITEM."
  (cddr item))

(defun rogue/item/equipped-p (item)
  "Whether ITEM is currently equipped."
  (or (eq item *rogue-player-weapon*)
      (seq-find (lambda (armor) (eq item armor))
                *rogue-player-armor*)))

;;; Weapons ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/weapon/make (name min-dmg max-dmg)
  "Create a weapon with a NAME as well as MIN-DMG and MAX-DMG values."
  (rogue/item/make 'WEAPON name min-dmg max-dmg))

(defvar +rogue-all-weapons+
  ;; TODO: Make useful list of weapons.
  '((SWORD 2 4)
    (CLUB 3 3)
    (SPOON 1 2)))

(make-variable-buffer-local '+rogue-all-weapons+)

(defun rogue/weapon/get (name)
  "Get the weapon with the right NAME."
  (let* ((weapon-stats
          (or (assoc name +rogue-all-weapons+)
              '(FIST 1 1))))
    (unless weapon-stats
      (error "Unknown weapon name '%s'. Available: %S"
             name
             (mapcar #'car +rogue-all-weapons+)))
    (apply #'rogue/weapon/make weapon-stats)))

(defun rogue/weapon/dmg (weapon)
  "The damage for the next attack with WEAPON."
  (cond ((null weapon) 1)
        ((rogue/item/weapon-p weapon)
         (let ((min-dmg (rogue/weapon/min-dmg weapon))
               (max-dmg (rogue/weapon/max-dmg weapon)))
           (+ min-dmg
              (random (+ 1 (- max-dmg min-dmg))))))
        (t (error "Item is not a weapon: %S" weapon))))

(defun rogue/weapon/min-dmg (weapon)
  "The minimal damage of WEAPON."
  (unless (eq (rogue/item/type weapon) 'WEAPON)
    (error "Item is not a weapon: %S" weapon))
  (car (rogue/item/specifics weapon)))

(defun rogue/weapon/max-dmg (weapon)
  "The maximal damage of WEAPON."
  (unless (eq (rogue/item/type weapon) 'WEAPON)
    (error "Item is not a weapon: %S" weapon))
  (cadr (rogue/item/specifics weapon)))

;;; Armor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/armor/make (name on-attacked)
  "Create a piece of armor with the given NAME and an ON-ATTACKED function.

The ON-ATTACKED function takes an initial damage and returns the possibly
reduced resulting damage. Other actions can be executed as well."
  (rogue/item/make 'ARMOR name on-attacked))

(defun rogue/armor/damage-reducer (amount)
  "Reduce incoming damage by AMOUNT."
  (lambda (damage) (max 0 (- damage amount))))

(defvar +rogue-all-armor+
  (list
   `(SHIELD ,(rogue/armor/damage-reducer 1))
   `(BLADE-MAIL
    ,(lambda (damage)
       (let ((returned 1))
         (rogue/monster/reduce-hp *rogue-current-monster* returned)
         (rogue/fight/add-to-log "%s takes %d damage"
                                 (rogue/monster/type *rogue-current-monster*)
                                 returned))
       (max 0 (- damage 2))))))

(make-variable-buffer-local '+rogue-all-armor+)

(defun rogue/armor/get (name)
  "Get the armor item with the right NAME."
  (let ((armor-stats
         (assoc name +rogue-all-armor+)))
    (unless armor-stats
      (error "Unknown armor name '%s'" name))
    (apply #'rogue/armor/make armor-stats)))

(defun rogue/armor/take-damage (armor damage)
  "Make use of ARMOR item when DAMAGE is taken."
  (unless (eq (rogue/item/type armor) 'ARMOR)
    (error "Item is not a weapon: %S" armor))
  (funcall (car (rogue/item/specifics armor)) damage))

;;; Consumables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/consumable/make (name on-consume)
  "Create a consumable item with the given NAME and an ON-CONSUME function."
  (rogue/item/make 'CONSUMABLE name on-consume))

(defvar +rogue-all-consumables+
  (list
   `(HEALTH-POTION ,(rogue/function/healer 2))
   `(MANA-POTION ,(rogue/function/manarest 4))))

(make-variable-buffer-local '+rogue-all-consumables+)

(defun rogue/consumable/get (name)
  "Get the armor item with the right NAME."
  (let ((consumable-stats
         (assoc name +rogue-all-consumables+)))
    (unless consumable-stats
      (error "Unknown consumable name '%s'" name))
    (apply #'rogue/consumable/make consumable-stats)))

;;; Spells ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rogue/spell/make (name mana in-dungeon in-combat )
  "Create spell with NAME and MANA cost, for actions IN-DUNGEON or IN-COMBAT.

The latter two arguments need to be callable functions without arguments. They
must return a description of the spell's effects."
  (list name mana in-dungeon in-combat))

(defun rogue/spell/learn (spell)
  "Make SPELL available to the player."
  (unless (seq-contains *rogue-player-available-spells* spell)
    (push spell *rogue-player-available-spells*)))

(defvar +rogue-all-spells+
  (list
   ;; TODO: Make useful list of spells
   (rogue/spell/make 'HEAL
                     2
                     (rogue/function/healer 3)
                     (rogue/function/healer 3))
   (rogue/spell/make 'LIGHTNING
                     3
                     #'rogue/function/only-in-combat
                     (rogue/function/damage-dealer 4))))

(make-variable-buffer-local '+rogue-all-spells+)

(defun rogue/spell/get (name)
  "Get the spell with the right NAME."
  (let ((spell (assoc name +rogue-all-spells+)))
    (or spell
        (error "Unknown spell '%s'" name))))

(defun rogue/spell/name (spell)
  "The name of SPELL."
  (car spell))

(defun rogue/spell/mana-cost (spell)
  "The mana cost of SPELL."
  (cadr spell))

(defun rogue/spell/ensure-enough-mana (spell)
  "Raise an error unless the player has enough mana to cast SPELL."
  (when (< *rogue-player-current-mana* (rogue/spell/mana-cost spell))
    (error "Not enough mana to cast %s: %d required"
           (rogue/spell/name spell)
           (rogue/spell/mana-cost spell))))

(defun rogue/spell/pay-mana-cost (spell)
  "Pay the mana cost for SPELL, lowering the player's mana pool."
  (setq *rogue-player-current-mana*
        (- *rogue-player-current-mana* (rogue/spell/mana-cost spell))))

(defun rogue/spell/cast-in-dungeon (spell)
  "Cast SPELL while exploring the dungeon."
  (rogue/spell/ensure-enough-mana spell)
  (rogue/message/set "%s" (funcall (nth 2 spell)))
  (rogue/spell/pay-mana-cost spell))

(defun rogue/spell/cast-in-combat (spell)
  "Cast SPELL while in a fight."
  (rogue/spell/ensure-enough-mana spell)
  (rogue/fight/add-to-log "You cast %s -- %s"
                          (rogue/spell/name spell)
                          (funcall (nth 3 spell)))
  (rogue/spell/pay-mana-cost spell))

(defun rogue/spell/active-p (spell)
  "Whether SPELL is currently active."
  (eq spell *rogue-player-spell*))

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
  "Whether positions POS-A and POS-B are in immediate proximity."
  (let ((x-diff (abs (- (rogue/pos/x pos-a) (rogue/pos/x pos-b))))
        (y-diff (abs (- (rogue/pos/y pos-a) (rogue/pos/y pos-b)))))
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
  "Whether any of SEQUENCES is empty."
  (cond
   ((null sequences) nil)
   ((null (car sequences)) t)
   (t (rogue/util/any-null-p (cdr sequences)))))

(defun rogue/util/sign (number)
  "The sign of NUMBER.

Returns -1/+1/0 if the argument is negative/positive/zero."
  (cond ((> number 0) 1)
        ((< number 0) -1)
        (t 0)))

;;; rogue.el ends here
