;;; -*- lexical-binding: t -*-
;;; alternate-region.el --- Make a second region, swap regions.

;;; Author: William Hatch <william@hatch.uno>
;;; Maintainer: William Hatch <william@hatch.uno>
;;; Version: 0.0
;;; Homepage: https://github.com/willghatch/emacs-alternate-region
;;; Git-Repository: git://github.com/willghatch/emacs-alternate-region.git
;;; Keywords: region
;;; Package-Requires: ((emacs "28"))

;;; License:
;;; This is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; <http://www.gnu.org/licenses/>

;;; Commentary:
;;; I wrote this package to support a workflow that I wanted: Often there is
;;; some piece of code that I want to lift out into a definition.  But then I
;;; need to decide where to put it.  I can cut the text first, put in the
;;; identifier I use, find a place to put it, start writing a definition and
;;; hope I use the same identifier, then paste it.  But maybe I'll get
;;; distracted along the way to finding the appropriate place.  And probably I
;;; won't remember the exact name I used.  So instead, with this package, I can
;;; set the region that I want to lift out as the alternate region with
;;; `alternate-region-activate', which gives it highlighting, then move on to
;;; find the right place to put the definition, write the name, copy the name,
;;; and paste the name on the right hand side, then select the name and use
;;; `alternate-region-swap' to move the regions.  Then fix indentation,
;;; probably.
;;;
;;; The API:
;;; • `alternate-region-activate' is interactive.  If `region-active-p', it sets the current region as the alternate region, and deactivates the region.  If not `region-active-p', deactivate the alternate region (setting it to nil).
;;; • `alternate-region-cycle' is interactive.  If `region-active-p' and there is an alternate region, it sets the current region as the alternate region and sets the previous alternate region as the new region.  Otherwise error.
;;; • `alternate-region-swap' is interactive.  If `region-active-p' and there is an alterante region, it swaps the contents of the region and the alternate region.  It updates the ranges of the region and alternate region, so they are both still active.  If the region and alternate region are not both active, error.
;;; • `alternate-region-set' takes a (cons beginning end) region or nil, and optional buffer (which defaults to the current buffer).  It accordingly activates or deactivates the alternate-region overlay.  Non-interactive.  Probably just use `alternate-region-activate' instead, but this is the function to use programatically.
;;; • 'alternate-region-face' is a face for the highlighted alternate region.

;;; Code:


;; TODO - I have no idea what the minimum emacs version required here is.  It can probably work with much earlier versions.
;; TODO - I would like to have multiple alternate regions in theory, but my main use case is just highlighting and swapping things.

(defvar alternate-region--current-list nil
  "List of alternate regions.
Each element is a list (BUFFER BEGIN END).")

(defface alternate-region-face-0
  '((default (:inherit region))
    (((background dark)) (:background "#105010"))
    (((background light)) (:background "#a0cfaf")))
  "Face for alternate-region index 0.")

(defface alternate-region-face-1
  '((default (:inherit region))
    (((background dark)) (:background "#501050"))
    (((background light)) (:background "#cfa0cf")))
  "Face for alternate-region index 1.")

(defface alternate-region-face-2
  '((default (:inherit region))
    (((background dark)) (:background "#501010"))
    (((background light)) (:background "#cfa0a0")))
  "Face for alternate-region index 2.")

(defface alternate-region-face-3
  '((default (:inherit region))
    (((background dark)) (:background "#505010"))
    (((background light)) (:background "#cfcfa0")))
  "Face for alternate-region index 3.")

(defface alternate-region-face-4
  '((default (:inherit region))
    (((background dark)) (:background "#105050"))
    (((background light)) (:background "#a0cfcf")))
  "Face for alternate-region index 4.")

(defface alternate-region-face-5
  '((default (:inherit region))
    (((background dark)) (:background "#301050"))
    (((background light)) (:background "#dfa0cf")))
  "Face for alternate-region index 5.")

(defface alternate-region-face-6
  '((default (:inherit region))
    (((background dark)) (:background "#503010"))
    (((background light)) (:background "#cfdfa0")))
  "Face for alternate-region index 6.")

(defface alternate-region-face-7
  '((default (:inherit region))
    (((background dark)) (:background "#103050"))
    (((background light)) (:background "#a0dfcf")))
  "Face for alternate-region index 7.")

(defface alternate-region-face-8
  '((default (:inherit region))
    (((background dark)) (:background "#301030"))
    (((background light)) (:background "#dfa0df")))
  "Face for alternate-region index 8.")

(defface alternate-region-face-9
  '((default (:inherit region))
    (((background dark)) (:background "#303010"))
    (((background light)) (:background "#dfdfa0")))
  "Face for alternate-region index 9.")

(defvar alternate-region--overlays nil
  "List of overlays for alternate regions.")

(defun alternate-region--get-face (index)
  "Get the face for alternate region at INDEX."
  (intern (format "alternate-region-face-%d" (mod index 10))))

(defun alternate-region--clear-all-overlays ()
  "Clear all alternate region overlays."
  (dolist (overlay alternate-region--overlays)
    (when overlay (delete-overlay overlay)))
  (setq alternate-region--overlays nil))

(defun alternate-region--update-overlays ()
  "Update overlays for all alternate regions."
  (alternate-region--clear-all-overlays)
  (let ((index 0))
    (dolist (region alternate-region--current-list)
      (let ((buffer (car region))
            (begin (cadr region))
            (end (caddr region)))
        (with-current-buffer buffer
          (let ((overlay (make-overlay begin end)))
            (overlay-put overlay 'face (alternate-region--get-face index))
            (setq alternate-region--overlays (cons overlay alternate-region--overlays))))
        (setq index (1+ index))))
    (setq alternate-region--overlays (nreverse alternate-region--overlays))))

(defun alternate-region-set (region &optional buffer)
  "Set the head of the alternate region list to REGION (a cons of BEGIN and END) in BUFFER.
If the list is empty, this pushes a new region."
  (with-current-buffer (or buffer (current-buffer))
    (if region
        (let* ((begin (car region))
               (end (cdr region))
               (new-region (list (current-buffer) begin end)))
          (if alternate-region--current-list
              (setcar alternate-region--current-list new-region)
            (setq alternate-region--current-list (list new-region)))
          (alternate-region--update-overlays)
          (add-hook 'after-change-functions 'alternate-region--update 10 t))
      (alternate-region-clear))))

(defun alternate-region-push (region &optional buffer)
  "Push REGION (a cons of BEGIN and END) to the front of the alternate region list in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (when region
      (let* ((begin (car region))
             (end (cdr region))
             (new-region (list (current-buffer) begin end)))
        (setq alternate-region--current-list (cons new-region alternate-region--current-list))
        (alternate-region--update-overlays)
        (add-hook 'after-change-functions 'alternate-region--update 10 t)))))

(defun alternate-region-clear ()
  "Clear all alternate regions."
  (interactive)
  (alternate-region--clear-all-overlays)
  (setq alternate-region--current-list nil)
  (remove-hook 'after-change-functions 'alternate-region--update t))

(defun alternate-region-list (&optional include-region)
  "Return the current list of alternate regions.
If INCLUDE-REGION is non-nil, include the current active region at the front."
  (if (and include-region (region-active-p))
      (cons (list (current-buffer) (region-beginning) (region-end))
            alternate-region--current-list)
    alternate-region--current-list))

(defun alternate-region--update (beg end prev-length)
  "Update alternate regions when a change is made in their buffer.
BEG and END indicate the boundaries of the changed region.
PREV-LENGTH is the length of the text that was in the modified region."
  (when alternate-region--current-list
    (let ((change-length (- (- end beg) prev-length))
          (updated-list nil)
          (needs-update nil))
      (dolist (region alternate-region--current-list)
        (let* ((alt-buffer (car region))
               (alt-start (cadr region))
               (alt-end (caddr region)))
          (if (eq (current-buffer) alt-buffer)
              (cond
               ((<= alt-end beg)
                (push region updated-list))
               ((<= end alt-start)
                (let ((new-alt-start (+ alt-start change-length))
                      (new-alt-end (+ alt-end change-length)))
                  (push (list alt-buffer new-alt-start new-alt-end) updated-list)
                  (setq needs-update t)))
               (t
                (setq needs-update t)))
            (push region updated-list))))
      (setq alternate-region--current-list (nreverse updated-list))
      (when needs-update
        (alternate-region--update-overlays)))))

(defun alternate-region-activate ()
  "Activate the alternate region by pushing the current active region to the list.  If not `region-active-p', clear all alternate regions."
  (interactive)
  (if (region-active-p)
      (let ((current-region (cons (region-beginning) (region-end))))
        (alternate-region-push current-region)
        (deactivate-mark))
    (alternate-region-clear)))

(defun alternate-region-cycle (&optional index)
  "Cycle between the current region and alternate regions.
INDEX can be:
- nil or 0: swap with head of alternate list (default behavior)
- number: swap with the alternate region at that index
- 'cycle: push current region to head, pop tail and activate it"
  (interactive "P")
  (cond
   ((eq index 'cycle)
    (if (and (region-active-p) alternate-region--current-list)
        (let ((current-region (cons (region-beginning) (region-end)))
              (tail-region (car (last alternate-region--current-list))))
          (alternate-region-push current-region)
          (setq alternate-region--current-list (butlast alternate-region--current-list))
          (let ((tail-buffer (car tail-region))
                (tail-start (cadr tail-region))
                (tail-end (caddr tail-region)))
            (when (not (eq (current-buffer) tail-buffer))
              (switch-to-buffer tail-buffer))
            (goto-char tail-start)
            (set-mark tail-end))
          (alternate-region--update-overlays))
      (error "Both current region and alternate regions must be active.")))
   (t
    (let ((target-index (or index 0)))
      (if (and (region-active-p)
               alternate-region--current-list
               (< target-index (length alternate-region--current-list)))
          (let* ((current-region (cons (region-beginning) (region-end)))
                 (target-region (nth target-index alternate-region--current-list))
                 (target-buffer (car target-region))
                 (target-start (cadr target-region))
                 (target-end (caddr target-region)))
            (setcar (nthcdr target-index alternate-region--current-list)
                    (list (current-buffer) (car current-region) (cdr current-region)))
            (when (not (eq (current-buffer) target-buffer))
              (switch-to-buffer target-buffer))
            (goto-char target-start)
            (set-mark target-end)
            (alternate-region--update-overlays))
        (error "Both current and alternate regions must be active, and index must be valid."))))))



(defun alternate-region-swap ()
  "Swap the contents of the current region and the head alternate region.  IE move the text between the two regions."
  (interactive)
  (if (and (region-active-p) alternate-region--current-list)
      (let* ((current-region (cons (region-beginning) (region-end)))
             (alt-region-data (car alternate-region--current-list))
             (alt-buffer (car alt-region-data))
             (alt-region (cons (cadr alt-region-data) (caddr alt-region-data)))
             (current-start (car current-region))
             (current-end (cdr current-region))
             (alt-start (car alt-region))
             (alt-end (cdr alt-region)))
        (if (equal (current-buffer) alt-buffer)
            ;; If both regions are in the same buffer
            (let ((current-text
                   (buffer-substring-no-properties current-start current-end))
                  (alt-text (buffer-substring-no-properties alt-start alt-end)))
              (delete-region current-start current-end)
              (goto-char current-start)
              (insert alt-text)
              ;; Update positions based on the current length
              (let* ((length-difference (- (length alt-text) (length current-text)))
                     (new-alt-start (+ (if (< current-start alt-start)
                                           length-difference 0)
                                       alt-start))
                     (new-alt-end (+ new-alt-start (length alt-text)))
                     (final-alt-end (+ new-alt-start (length current-text))))
                (delete-region new-alt-start new-alt-end)
                (goto-char new-alt-start)
                (insert current-text)
                ;; Update the head of the alternate region list
                (setcar alternate-region--current-list
                        (list alt-buffer new-alt-start final-alt-end))
                (alternate-region--update-overlays)
                (goto-char (+ current-start (length alt-text)))
                (set-mark current-start)))
          ;; If regions are in different buffers
          (let ((current-text
                 (buffer-substring-no-properties current-start current-end))
                (alt-text (with-current-buffer alt-buffer
                            (buffer-substring-no-properties alt-start alt-end))))
            ;; Swap text in the current buffer
            (delete-region current-start current-end)
            (goto-char current-start)
            (insert alt-text)
            (goto-char (+ current-start (length alt-text)))
            (set-mark current-start)
            ;; Swap text in the alternate buffer
            (with-current-buffer alt-buffer
              (delete-region alt-start alt-end)
              (goto-char alt-start)
              (insert current-text))
            ;; Update the head of the alternate region list
            (setcar alternate-region--current-list
                    (list alt-buffer alt-start (+ alt-start (length current-text))))
            (alternate-region--update-overlays))))
    (error "Both current and alternate regions must be active.")))

(provide 'alternate-region)
