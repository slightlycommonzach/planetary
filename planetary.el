;;; planetary.el -- Solar System Scroll indication -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.
 
;;; Commentary:

;; WoOOOoOOoOo

;;; Code:

(eval-when-compile '(require 'cl))

(defconst planetarium-root (file-name-directory load-file-name))

(defgroup planetary nil
  "Customization group for Planetary."
  :tag "Planetarium!"
  :group 'frames)

(defun restart ()
  "Refresh after option change if loaded."
  (when (featurep 'planetary-mode)
    (when (and (boundp 'planetary-mode)
               planetary-mode)
      (planetary-mode -1)
      (planetary-mode 1))))

(defcustom allowed-axis t
  "Axis? Decide if you just want the planets."
  :tag "Solar System Revolution Axis"
  :type 'boolean
  :set (lambda (sym val)
		 (set-default sym val)
		 (restart))
  :group 'planetary)

(defcustom dark-planets t
  "Replace the empty space with dark representations found in `dark-planets/'."
  :tag "Graphical click indicator"
  :type 'boolean
  :set (lambda (sym val)
		 (set-default sym val)
		 (restart))
  :group 'planetary)

(defcustom exclude-similar-data nil
  "Use this option to remove the \"L#\" and the \"C#\" in the modeline."
  :type 'boolean
  :set (lambda (sym val)
		 (set-default sym val)
		 (restart))
  :group 'planetary)

(defcustom image-root (concat planetarium-root "images/")
  "User can select an alternative image directory."
  :type 'string
  :set (lambda (sym val)
         (set-default sym val)
         (restart))
  :group 'planetary)

(defcustom debug-mode nil
  "Per vertical cursor movement, print data attached to the amount to print."
  :type 'boolean
  :set (lambda (sym val)
		 (set-default sym val)
		 (restart))
  :group 'planetary)

(defcustom min-buffer-width 64
  "Minimum size of a buffer window to display the Planetary modeline feature."
  :type 'integer
  :set (lambda (sym val)
         (set-default sym val)
         (restart))
  :group 'planetary)

(defcustom min-buffer-text " Planetary"
  "If the buffer window width is less than the min-buffer-width variable (defaulted at 64), replace the default \"*invalid*\" with a text of your choice."
  :tag "Hint: Use (propertize)"
  :type 'string
  :set (lambda (sym val)
		 (set-default sym val)
		 (restart))
  :group 'planetary)

(defcustom animated-timer nil
  "Enable a timer that will flash asteroids in place of the symbolic solar system white axis, textbook style."
  :type 'boolean
  :set (lambda (sym val)
		 (set-default sym val)
		 (restart)
		 (if (eq animated-timer t)
			 (start-blink-timer)))
  :group 'planetary)

(defcustom stand-up-timer "30 min"
  "Time to wait to start the animation repeatedly."
  :type 'string
  :set (lambda (sym val)
		 (set-default sym val)
		 (restart))
  :group 'planetary)

(defvar count-it 0)
(defvar proper-axis nil)
(defvar stand-up-reminder-timer nil)
(defvar is-timing-p (timerp stand-up-reminder-timer))
(defvar is-animating-p nil)

(defun get-proper-axis ()
  "Alternate between the contrast and normal based on odds/evens."
  (if is-animating-p
	  (let ((result '()))
		(if (eq (% count-it 2) 0)
			(setq result (create-image (get-extension (concat image-root "normal-standards/normal-flash")) (intern (file-name-extension (get-extension (concat image-root "normal-standards/normal-flash"))))))
		  (setq result (create-image (get-extension (concat image-root "dark-standards/contrast-flash")) (intern (file-name-extension (get-extension (concat image-root "dark-standards/contrast-flash")))))))
		(setq count-it (1+ count-it))
		result)))

(defun get-update ()
  "Using get-proper-axis, the program can use this function to set the alteration."
  (setq is-animating-p t
		proper-axis (get-proper-axis))
  (force-mode-line-update))

(defun start-blink ()
  "Override the timer to display the flash itself, not activating the timer."
  (interactive)
  (if (timerp stand-up-reminder-timer)
	  (get-update)
	(setq stand-up-reminder-timer (run-at-time "1 sec" .4 (function (lambda () (get-update)))))))

(defun start-blink-timer ()
  "Start the timer that after an alotted time `stand-up-timer', will blink an asteroid in contrast to normal as a reminder to stand up and stretch."
  (interactive)
  (if (not is-timing-p)
	  (progn
		(setq stand-up-reminder-timer (run-at-time stand-up-timer .4 (function start-blink)))
		(setq is-timing-p (timerp stand-up-reminder-timer))
		(message "Blinking enabled, toggle it with M-x \"stop-blink\" again to turn it off."))
	(message "Timer already running, set to 30 mins by default.")))

(defun stop-blink ()
  "Stop the actual animation from blinking, restart the timer."
  (interactive)
  (if (timerp stand-up-reminder-timer)
	  (progn
		(cancel-timer stand-up-reminder-timer)
		(setq stand-up-reminder-timer nil
			  proper-axis nil
			  is-animating-p nil)
		(force-mode-line-update)
		(if is-timing-p
			(setq stand-up-reminder-timer (run-at-time stand-up-timer .3 (function (lambda () (start-blink)))))))
	(message "Not running.")))

(defun count-planets (dir)
  "Count primary images in DIR so long as they follow the directed standard."
  (let* ((files (directory-files dir nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
		 (count 0))
	(dotimes (number (length files))
	  (if (string-match-p "\\`[0-9]*\\'" (file-name-sans-extension (elt files number)))
		  (setq count (+ count 1))))
	count))

(defconst solar-system-length (count-planets (concat image-root "normal-standards/")))
(defconst turning-off-mlp mode-line-position) ; Constant definition of the old mode-line-position (being the X% and L#), as to seamlessly turn off planetary mode interactively.

;;;###autoload
(define-minor-mode planetary-mode
  "Buffer length representational of the distance of a users screen into a given buffer."
  :global t
  :init-value nil
  :lighter " Planetarium"
  :group 'planetary
  (if planetary-mode
	  (if exclude-similar-data
		  (setq mode-line-position '(:eval (create-planetarium)))
		(setq mode-line-position (list mode-line-position '(:eval (create-planetarium)))))
	(setq mode-line-position turning-off-mlp)))

(defun activated-planets ()
  "Calculate the divisor of the images, what % of the buffer needs to be traversed through to get another one BOOL."
  (let ((result 0))
	(setq result (round
				  (/ (* (/ (float (point)) (point-max))
						(* 100 solar-system-length))
					 100)))
    result))

(defun find-duplicates (dir)
  "Given DIR, Crop out the file name that might be associated to get the latest folder in it and check for duplicates so that our compatibility approach isnt tainted."
  (let ((files (directory-files (substring dir 0 (+ (length "standards/") (cl-search "standards/" dir :test 'equal))) nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
		(result '()))
    (if (not (equal (cl-remove-duplicates files) files))
		(setq result t)
	  (setq result nil))
	result))

(defun get-appropriate-axis (string axis-cond-test dir)
  "STRING AXIS-COND-TEST DIR."
  (if (image-type-available-p (intern (file-name-extension dir)))
	  (if (and (eq proper-axis nil) (not is-animating-p))
		  (propertize string 'display (create-image (get-extension (concat image-root axis-cond-test "-standards/axis"))
													(intern (file-name-extension (get-extension (concat image-root axis-cond-test "-standards/axis"))))
													nil :ascent 'center))
		(propertize string 'display proper-axis))
	(throw 'error "Notice: Double-check support for the images you added.")))

(defun buttonize (string number)
  "Alter STRING to add a button ui such that it will move to NUMBERs ratio to solar-system-length in comparison to the length of the buffer, and move to that point."
  (propertize string
			  'mouse-face 'mode-line-highlight
			  'local-map (make-mode-line-mouse-map 'mouse-1 (lambda () (interactive)
															  (with-current-buffer (current-buffer)
																(goto-char (round (* (/ (float number)
																						solar-system-length)
																					 (point-max)))))))))

(defun appropriate-image (string dir &optional number)
  "Using STRING as a fallback value, create an image of DIR and display it on the mode line based on position NUMBER, the inner workings whilst `create-planetarium' is the foundation."
  (let* ((result "")
		 (file-type (intern (file-name-extension dir)))
		 (loop-num (file-name-base dir))
		 (n-or-d (if (eq (cl-search "normal-standards/" dir :test 'equal) nil) "dark"
				   "normal")))
	(if (image-type-available-p file-type)
		(progn
		  (if (and allowed-axis
				   (not (string= dir (get-extension (concat image-root n-or-d "-standards/0")))))
			  (setq result (get-appropriate-axis string n-or-d dir)))
		  (setq result (concat result
							   (buttonize
									   (propertize string 'display (create-image dir file-type nil :ascent 'center))
									   (if (not number) (string-to-number loop-num)
										 number)))))
	  (throw 'warning "Notice: Double-check support for the images you added."))
	result))

(defconst planetarium--has-duplicates (if (or (and (not (find-duplicates  (concat image-root "normal-standards/")))
												   (not dark-planets))
											  (and (not (find-duplicates (concat image-root "normal-standards/")))
												   (not (find-duplicates (concat image-root "dark-standards/")))))
										  t
										nil))

(defun get-extension (dir)
  "Allow the user to input an image of whatever kind they may be interested in DIR by grabbing its extension out of the folder its in by finding the name and isolating the extension."
  (let ((files (directory-files (file-name-directory dir) nil "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
		(result ""))
	(dotimes (number (length files)) ;; Sets the argument to the full name, searches the list of file names to get the extension to allow user-level compatibility.
	  (if (string= (file-name-base (elt files number)) (file-name-base dir))
		  (setq result (concat "." (file-name-extension (elt files number))))))
	(setq result (concat dir result))
	(setq result (substring result (cl-search image-root dir :test 'equal) (length result)))
	result))

(defun create-planetarium ()
  "The central are for collecting data from the various functions."
  (if planetarium--has-duplicates
	  (if (or (and dark-planets
				   (eq (count-planets (concat image-root "normal-standards/"))
					   (count-planets (concat image-root "dark-standards/"))))
			  (eq dark-planets nil))
		  (if (> (window-width) min-buffer-width)
			  (let* ((result "")
					 (planets (activated-planets))
					 (moreclick-size (- solar-system-length planets))) ;; Get the remainder, the solar-system-length is simply the size of the numerically named photos found in `normal-standards/', and planets is just the amount to activate (pull from `normal-standards/')
				(if debug-mode (message (format "Planets: %s | MoreClick-Size %s | Solar-System-Length: %s" planets moreclick-size solar-system-length)))
 				(dotimes (number planets)
				  (setq result (concat result (appropriate-image "_" (get-extension (concat image-root (format "normal-standards/%d" number)))))))
				(dotimes (number moreclick-size)
				  (setq result (concat result
									   (if dark-planets
										   (appropriate-image "_" (get-extension (format (concat image-root "dark-standards/%d") (+ number planets))))
										 (progn
										   (appropriate-image "_" (get-extension (concat image-root "empty")) (+ planets number)))))))
			  (propertize (concat "{" result "}") 'help-echo "WoOOOoOOoOo\nmouse-1: Scroll vertically with a click."))
			min-buffer-text) ; Replaces "*invalid*"
		(throw 'error "Unequal amounts of images in `dark-standards/' and `normal-standards/' to be represented, either disable `dark-planets' or fix the issue."))
	(throw 'error "Please rename your images with the same prefix so our mode doesn't confuse the 2.")))

(provide 'planetary)
;;; planetary.el ends here
