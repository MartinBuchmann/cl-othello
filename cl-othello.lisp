;;;; cl-othello.lisp

(in-package #:cl-othello)

;;; cl-othello.lisp Meine Variante eines Othello-Spiels implementiert
;;; in Common Lisp.  Inspiriert von Peter Norvig.

;;; Die Konstanten und eigenen Datentypen

;;; Das Spielfeld ist ein Vektor mit 100 Werten (10x10 Feldern), um
;;; die Ränder gesondert behandeln zu können.  das eigentliche
;;; Spielfeld hat 8x8 Felder.  Es gibt somit vier Zustände eines
;;; Feldes: leer, weiß, schwarz oder draußen.
(defconstant leer    0 "Ein leeres Feld")
(defconstant schwarz 1 "Ein schwarzer Stein")
(defconstant weiß    2 "Ein weißer Stein")
(defconstant draußen 3 "Markiert die felder außerhalb des 8x8 Feldes.")

(defvar *zug-nummer* 1 "Die Nummer des nächsten Zugs")
(defvar *zeit* (make-array 3) "Die Spielzeit")

;;; Ein neuer Datentyp für die Felder
(deftype feld () `(integer ,leer ,draußen))

;;; Das Spielfeld ist ein Vektor
(deftype spielfeld () '(simple-array feld (100)))

;;; Kodiert die Werte für die Felder
(defun name (feld) (char ".@O?" feld))

;;; Gibt die Farbe des jeweiligen Gegeners zurück
(defun gegner (spieler) (if (eql spieler schwarz) weiß schwarz))

;;; Die Richtungen auf dem Spielfeld
(defparameter alle-richtungen '(-11 -10 -9 -1 1 9 10 11))

;;; Eine Liste aller gültigen Felder
(defparameter alle-felder
  (loop for i from 11 to 88 when (<= 1 (mod i 10) 8) collect i))

(defun neues-spielfeld ()
  "Gibt ein neues Spielfeld zurück, d.h. alle Felder sind leer; die
äußeren mit DRAUßEN gekennzeichnet und die vier im Zentrum besetzt."
  (let ((spielfeld (make-array 100 :element-type 'feld
                           :initial-element draußen)))
    (dolist (feld alle-felder)
      (setf (aref spielfeld feld) leer))
    (setf (aref spielfeld 44) weiß   (aref spielfeld 45) schwarz
          (aref spielfeld 54) schwarz   (aref spielfeld 55) weiß)
    spielfeld))

(defvar *spielfeld* (neues-spielfeld) "Das Spielfeld")

(defun spielstand (spieler spielfeld)
  "Zählt alle Steine und bildet die Differenz mit den Steinen des Gegners."
  (- (count spieler spielfeld)
     (count (gegner spieler) spielfeld)))

(defun gültig-p (zug)
  "Gültige Züge sind Zahlen zwischen 11 und 88, deren Einerstelle
zwischen 1 und 8 liegt."
  (and (integerp zug) (<= 11 zug 88) (<= 1 (mod zug 10) 8)))

(defun erlaubt-p (zug spieler spielfeld)
  "Ein erlaubter Zug muss auf ein leeres Feld landen und mindestens
einen gegnerischen Stein umdrehen."
  (and (eql (aref spielfeld zug) leer)
       (some #'(lambda (richtung) (umdrehen-p zug spieler spielfeld richtung))
             alle-richtungen)))

(defun umdrehen-p (zug spieler spielfeld richtung)
  "Würde ZUG ein Umdrehen von gegnerischen Steinen in RICHTUNG
  erlauben? Wenn ja, gibt die Nummer des Feldes zurück."
  (let ((c (+ zug richtung)))
    (and (eql (aref spielfeld c) (gegner spieler))
         (finde-eingrenzenden-stein (+ c richtung) spieler spielfeld richtung))))

(defun finde-eingrenzenden-stein (feld spieler spielfeld richtung)
  "Gibt die Nummer des Feldes des eingrenzenden Steins zurück."
  (cond ((eql (aref spielfeld feld) spieler) feld)
        ((eql (aref spielfeld feld) (gegner spieler))
         (finde-eingrenzenden-stein (+ feld richtung) spieler spielfeld richtung))
        (t nil)))

(defun ziehe (zug spieler spielfeld)
  "Aktualisiert das SPIELFELD um ZUG von SPIELER"
  (setf (aref spielfeld zug) spieler)
  (dolist (richtung alle-richtungen)
    (drehe zug spieler spielfeld richtung))
  spielfeld)

(defun drehe (zug spieler spielfeld richtung)
  "Drehe alle möglichen Steine in der gegebenden Richtung."
  (let ((begrenzer (umdrehen-p zug spieler spielfeld richtung)))
    (when begrenzer
      (if (minusp richtung)
          ;; Ändert die Richtung, damit loop nicht wegen der negativen Schritte meckert
          (loop :with richtung = (* -1 richtung)
                :for c :from (+ begrenzer richtung) :by richtung :until (eql c zug)
                do (setf (aref spielfeld c) spieler))
          (loop :for c :from (+ zug richtung) :by richtung :until (eql c begrenzer)
                do (setf (aref spielfeld c) spieler))))))

(defun nächster (spielfeld vorgänger anzeige)
  "Berechnet den nächsten Spieler oder gibt NIL zurück, wenn keiner ziehen kann."
  (let ((gegner (gegner vorgänger)))
    (cond ((erlaubte-züge? gegner spielfeld) gegner)
          ((erlaubte-züge? vorgänger spielfeld) 
           (when anzeige
             (format t "~&~c hat keine möglichen Züge und muss aussetzen."
                     (name gegner))
             (finish-output))
           vorgänger)
          (t nil))))

(defun erlaubte-züge? (spieler spielfeld)
  "Gibt es erlaubte Züge für SPIELER in dieser Position?"
  (some #'(lambda (zug) (erlaubt-p zug spieler spielfeld))
        alle-felder))

(defun erlaubte-züge (spieler spielfeld)
  "Gibt eine Liste aller erlaubten Züge zurück."
  (loop :for zug :in alle-felder
	:when (erlaubt-p zug spieler spielfeld) :collect zug))

(defun zufalls-zug (spieler spielfeld)
  "Macht einen zufälligen, erlaubten Zug."
  (let ((erlaubte-züge (erlaubte-züge spieler spielfeld)))
    (nth (random (length erlaubte-züge)) erlaubte-züge)))

;;; Die Tabelle mit den benachbarten Feldern
(let ((nachbar-tabelle (make-array 100 :initial-element nil)))
  ;; Initialisierung der Tabelle
  (dolist (feld alle-felder)
    (dolist (richtung alle-richtungen)
      (if (gültig-p (+ feld richtung))
          (push (+ feld richtung)
                (aref nachbar-tabelle feld)))))

  (defun nachbarn (feld)
    "Gibt eine Liste aller Nachbarfelder zurück."
    (aref nachbar-tabelle feld)))

(let ((feld-namen
        (alexandria:mappend
         #'(lambda (y)
             (mapcar #'(lambda (x) (alexandria:format-symbol t "~{~a~}" (list x y)))
                     '(? a b c d e f g h ?)))
         '(? 1 2 3 4 5 6 7 8 ?))))

  (defun h8->88 (text)
    "Konvertiert den TEXT in die numerische Feld-Notation."
    (or (position (string text) feld-namen :test #'string-equal)
        text))

  (defun 88->h8 (nummer)
    "Konvertiert die Nummer in die alphanumerische Feld-Notation."
    (if (gültig-p nummer)
        (elt feld-namen nummer)
        nummer)))

(defun mensch (spieler spielfeld)
  "Eingabe eines Spielers für ein Othello-Spiel."
  (format t "~&~c kann folgende Züge machen ~a: " (name spieler)
          (mapcar #'88->h8 (erlaubte-züge spieler spielfeld)))
  (finish-output)
  (h8->88 (read-line *query-io*)))

(defun othello (s-strategy w-strategy 
                &optional (ausgabe t) (minuten 30))
  "Spielt Othello, gibt den Punktestand aus, wobei ein positives Ergebnis für
  Schwarz zählt."
  (let ((spielfeld (neues-spielfeld))
        (zeit (make-array (+ 1 (max schwarz weiß))
                           :initial-element 
                           (* minuten 60 
                              internal-time-units-per-second))))
    (catch 'game-over
      (loop :for *zug-nummer* :from 1
            :for spieler = schwarz :then (nächster spielfeld spieler ausgabe)
            :for strategie = (if (eql spieler schwarz) 
                               s-strategy
                               w-strategy)
            :until (null spieler)
            :do (nächster-zug strategie spieler spielfeld ausgabe zeit))
      (when ausgabe
        (format t "~&Das Spiel ist aus. Endergebnis:")
        (finish-output)
        (gib-feld-aus spielfeld zeit))
      (spielstand schwarz spielfeld))))

(defun nächster-zug (strategie spieler spielfeld ausgabe zeit)
  "Berechnet den nächsten Zug abhängig von der gewählten Strategie."
  (when ausgabe (gib-feld-aus spielfeld zeit))
  (replace *zeit* zeit)
  (let* ((t0 (get-internal-real-time))
         (zug (funcall strategie spieler (replace *spielfeld* spielfeld)))
         (t1 (get-internal-real-time)))
    (decf (elt zeit spieler) (- t1 t0))
    (cond
      ((< (elt zeit spieler) 0)
       (format t "~&~c hat seine Zeit verbraucht."
               (name spieler))
       (finish-output)
       (THROW 'game-over (if (eql spieler schwarz) -64 64)))
      ((eq zug 'resign)
       (THROW 'game-over (if (eql spieler schwarz) -64 64)))
      ((and (gültig-p zug) (erlaubt-p zug spieler spielfeld))
       (when ausgabe
         (format t "~&~c zieht nach ~a." 
                 (name spieler) (88->h8 zug))
         (finish-output))
       (ziehe zug spieler spielfeld))
      (t (warn "Ungültiger Zug: ~a" (88->h8 zug))
         (nächster-zug strategie spieler spielfeld ausgabe zeit)))))

(defun gib-feld-aus (&optional (spielfeld *spielfeld*) zeit)
  "Gibt das Spielfeld und die Statisken aus."
  ;; Der aktuelle Punktestand
  (format t "~2&    a b c d e f g h   [~c=~2a ~c=~2a (~@d)]"
          (name schwarz) (count schwarz spielfeld)
          (name weiß) (count weiß spielfeld)
          (spielstand schwarz spielfeld))
  (finish-output)
  ;; Das Spielfeld
  (loop :for reihe :from 1 :to 8 :do
    (format t "~&  ~d " reihe)
    (finish-output)
    (loop :for spalte :from 1 :to 8
          :for feld = (aref spielfeld (+ spalte (* 10 reihe)))
          :do (format t "~c " (name feld))))
  ;; Die verbleibende Zeit pro Spieler
  (when zeit
    (format t "  [~c=~a ~c=~a]~2&"
            (name schwarz) (uhr (elt zeit schwarz))
            (name weiß) (uhr (elt zeit weiß))))
  (finish-output))

(defun uhr (zeit)
  "Gibt einen String der verbleibenden Spielzeit in min:sec zurück."
  (multiple-value-bind (min sec)
      (floor (round zeit internal-time-units-per-second) 60)
    (format nil "~2d:~2,'0d" min sec)))
