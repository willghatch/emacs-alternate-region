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
;;; • `alternate-region-activate' is interactive. If `region-active-p', it pushes the current region to the alternate region list and deactivates the region. If not `region-active-p', clears all alternate regions.
;;; • `alternate-region-cycle' is interactive. Takes optional argument: nil/0 to swap with head (default), number to swap with region at index, or 'cycle to push current to head and pop tail to activate.
;;; • `alternate-region-swap' is interactive. If `region-active-p', swaps current region with head alternate region. If not `region-active-p', swaps the first two alternate regions.
;;; • `alternate-region-push' takes a (cons beginning end) region and optional buffer, pushes it to the front of the alternate region list. Non-interactive.
;;; • `alternate-region-pop' pops the top alternate region and returns it.
;;; • `alternate-region-set' takes a (cons beginning end) region and optional buffer, replaces the head of the alternate region list (or pushes if empty). Non-interactive.
;;; • `alternate-region-pop-go' is interactive. Pops the top alternate region, sets it as current region, and returns it.
;;; • `alternate-region-clear' clears all alternate regions.
;;; • `alternate-region-list' takes optional include-region argument. Returns the list of alternate regions, optionally with current region at front.
;;; • 'alternate-region-face-0' through 'alternate-region-face-9', faces for the highlighted alternate regions.

;;; Code:


;; TODO - I have no idea what the minimum emacs version required here is.  It can probably work with much earlier versions.

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

(defun alternate-region-pop ()
  "Pop the top alternate region from the list and return it.
Returns the region in the format (BUFFER BEGIN END), or nil if list is empty."
  (when alternate-region--current-list
    (let ((popped-region (car alternate-region--current-list)))
      (setq alternate-region--current-list (cdr alternate-region--current-list))
      (alternate-region--update-overlays)
      popped-region)))

(defun alternate-region-pop-go ()
  "Pop the top alternate region from the list, set it as current region, and return it.
Returns the region in the format (BUFFER BEGIN END), or nil if list is empty."
  (interactive)
  (let ((popped-region (alternate-region-pop)))
    (when popped-region
      (let ((popped-buffer (car popped-region)))
        (when (not (eq (current-buffer) popped-buffer))
          (switch-to-buffer popped-buffer))
        (goto-char (caddr popped-region))
        (set-mark (cadr popped-region))
        (activate-mark)
        popped-region))))

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

(defun alternate-region--exchange (region1 region2)
  "Exchange the contents of REGION1 and REGION2.
Both regions should be in the format (BUFFER BEGIN END).
Returns a list (NEW-REGION1 NEW-REGION2) with updated positions."
  (let* ((buffer1 (car region1))
         (start1 (cadr region1))
         (end1 (caddr region1))
         (buffer2 (car region2))
         (start2 (cadr region2))
         (end2 (caddr region2)))
    (with-undo-amalgamate
      (if (equal buffer1 buffer2)
          ;; If both regions are in the same buffer
          (with-current-buffer buffer1
            (let ((text1 (buffer-substring-no-properties start1 end1))
                  (text2 (buffer-substring-no-properties start2 end2)))
              (delete-region start1 end1)
              (goto-char start1)
              (insert text2)
              ;; Update positions based on the current length
              (let* ((length-difference (- (length text2) (length text1)))
                     (new-start2 (+ (if (< start1 start2) length-difference 0)
                                    start2))
                     (new-end2 (+ new-start2 (length text2)))
                     (final-end2 (+ new-start2 (length text1))))
                (delete-region new-start2 new-end2)
                (goto-char new-start2)
                (insert text1)
                ;; Calculate final positions
                (let ((final-start1 (if (< start1 start2)
                                        start1
                                      (+ start1 (- length-difference))))
                      (final-end1 (if (< start1 start2)
                                      (+ start1 (length text2))
                                    (+ start1
                                       (- length-difference)
                                       (length text2)))))
                  (list (list buffer1 final-start1 final-end1)
                        (list buffer2 new-start2 final-end2))))))
        ;; If regions are in different buffers
        (let ((text1 (with-current-buffer buffer1
                       (buffer-substring-no-properties start1 end1)))
              (text2 (with-current-buffer buffer2
                       (buffer-substring-no-properties start2 end2))))
          ;; Swap text in buffer1
          (with-current-buffer buffer1
            (delete-region start1 end1)
            (goto-char start1)
            (insert text2))
          ;; Swap text in buffer2
          (with-current-buffer buffer2
            (delete-region start2 end2)
            (goto-char start2)
            (insert text1))
          ;; Return new regions
          (list (list buffer1 start1 (+ start1 (length text2)))
                (list buffer2 start2 (+ start2 (length text1)))))))))

(defun alternate-region-swap ()
  "Swap the contents of regions.
If current region is active, swap it with the head alternate region.
If current region is inactive, swap the first two alternate regions."
  (interactive)
  (cond
   ((and (region-active-p) alternate-region--current-list)
    ;; Swap current region with head alternate region
    (let* ((current-region-data (list (current-buffer) (region-beginning)
                                      (region-end)))
           (alt-region-data (car alternate-region--current-list))
           (exchanged-regions
            (alternate-region--exchange current-region-data alt-region-data))
           (new-current-region (car exchanged-regions))
           (new-alt-region (cadr exchanged-regions)))
      ;; Update the head of the alternate region list
      (setcar alternate-region--current-list new-alt-region)
      (alternate-region--update-overlays)
      ;; Set the region to the newly swapped text
      (let ((new-current-buffer (car new-current-region))
            (new-current-start (cadr new-current-region))
            (new-current-end (caddr new-current-region)))
        (goto-char new-current-end)
        (set-mark new-current-start)
        (activate-mark))))
   ((and (not (region-active-p))
         alternate-region--current-list
         (>= (length alternate-region--current-list) 2))
    ;; Swap the first two alternate regions
    (let* ((first-alt (car alternate-region--current-list))
           (second-alt (cadr alternate-region--current-list))
           (current-point (point))
           (current-buffer (current-buffer))
           (first-buffer (car first-alt))
           (first-start (cadr first-alt))
           (first-end (caddr first-alt))
           (second-buffer (car second-alt))
           (second-start (cadr second-alt))
           (second-end (caddr second-alt))
           (exchanged-regions
            (alternate-region--exchange first-alt second-alt))
           (new-first-alt (car exchanged-regions))
           (new-second-alt (cadr exchanged-regions)))
      ;; Update the first two elements of the alternate region list
      (setcar alternate-region--current-list new-first-alt)
      (setcar (cdr alternate-region--current-list) new-second-alt)
      (alternate-region--update-overlays)
      ;; Calculate and set the new cursor position
      (cond
       ((eq current-buffer first-buffer)
        (let ((new-point
               (cond
                ;; Point is before both regions (if second is in same buffer)
                ((and (eq first-buffer second-buffer)
                      (<= current-point (min first-start second-start)))
                 current-point)
                ;; Point is after both regions (if second is in same buffer)
                ((and (eq first-buffer second-buffer)
                      (>= current-point (max first-end second-end)))
                 current-point)
                ;; Point is between the regions (if second is in same buffer)
                ((and (eq first-buffer second-buffer)
                      (>= current-point (min first-end second-end))
                      (<= current-point (max first-start second-start)))
                 (+ current-point
                    (if (< first-start second-start)
                        (- (cadr new-second-alt) second-start)
                      (- (cadr new-first-alt) first-start))))
                ;; Point is within the first region
                ((and (>= current-point first-start) (<= current-point first-end))
                 (+ (cadr new-first-alt) (- current-point first-start)))
                ;; Point is within the second region (if in same buffer)
                ((and (eq first-buffer second-buffer)
                      (>= current-point second-start) (<= current-point second-end))
                 (+ (cadr new-second-alt) (- current-point second-start)))
                ;; Point is before/after first region only (second region in different buffer)
                ((<= current-point first-start)
                 current-point)
                ((>= current-point first-end)
                 (+ current-point (- (caddr new-first-alt) first-end)))
                ;; Default case
                (t current-point))))
          (goto-char new-point)))
       ((eq current-buffer second-buffer)
        (let ((new-point
               (cond
                ;; Point is within the second region
                ((and (>= current-point second-start) (<= current-point second-end))
                 (+ (cadr new-second-alt) (- current-point second-start)))
                ;; Point is before/after second region
                ((<= current-point second-start)
                 current-point)
                ((>= current-point second-end)
                 (+ current-point (- (caddr new-second-alt) second-end)))
                ;; Default case
                (t current-point))))
          (goto-char new-point))))))
   (t
    (error "Either current region must be active with at least one alternate region, or at least two alternate regions must exist."))))

(provide 'alternate-region)
