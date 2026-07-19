;;; gnus-gnaw.el --- Highlight BONE reports -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Bastien Guerry

;; Author: Bastien Guerry <bzg@gnu.org>
;; Maintainer: Bastien Guerry <bzg@gnu.org>
;; Keywords: news, mail
;; URL: https://codeberg.org/bzg/gnus-gnaw
;; Version: 0.13.0
;; Package-Requires: ((emacs "28.1") (gnaw "0.3"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This library is not actively maintained, it is shared as a proof of
;; concept.  If you want to maintain and develop it, please contact me.
;;
;; M-x gnus-gnaw RET will limit summary to BONE reports + highlight
;; M-x gnus-gnaw-highlight RET will highlight BONE reports (no limit)
;; M-x gnus-gnaw-topic RET filters highlighted reports by topic
;; M-x gnus-gnaw-clear RET will unhighlight and disable auto-rehighlight
;; M-x gnaw-update RET will force update of the remote reports cache
;;
;; The following commands toggle gnaw's local marks (kept in
;; ~/.config/gnaw/state.edn so they are shared with the gnaw CLI):
;;
;; M-x gnus-gnaw-mark-sticky RET -- toggle the sticky mark (keep visible)
;; M-x gnus-gnaw-mark-dismiss RET -- toggle the dismiss mark (hide)
;;
;; The annotation gains a leading mark column: '!' = sticky, 'd' = dismiss.
;;
;; gnus-gnaw builds on the `gnaw' library for the shared data layer
;; (configuration, report sources, cache and state.edn); this file only
;; provides the Gnus presentation and commands.
;;
;;; Code:

(require 'gnaw)

(declare-function gnus-summary-article-number "gnus-sum")
(declare-function gnus-summary-article-header "gnus-sum")
(declare-function gnus-summary-limit "gnus-sum")
(declare-function gnus-summary-pop-limit "gnus-sum")
(declare-function mail-header-id "nnheader")

(defgroup gnus-gnaw nil
  "Highlight BONE reports in Gnus summary buffers."
  :group 'gnus)

(defface gnus-gnaw-face
  '((((background light)) :background "#e8e8e8")
    (((background dark))  :background "#333333"))
  "Subtle highlight for BONE reports in Gnus summary."
  :group 'gnus-gnaw)

(defface gnus-gnaw-annotation-face
  '((t :inherit shadow))
  "Face for right-margin annotations."
  :group 'gnus-gnaw)

;;; Summary overlays

(defun gnus-gnaw--for-each-summary-mid (fn)
  "Map FN over article numbers and normalized MIDs in summary buffer.
The MID passed to FN is normalized with `gnaw-normalize-mid', matching the
keys gnaw uses for both reports and state."
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let* ((article (gnus-summary-article-number))
             (header  (and (numberp article) (> article 0)
                           (gnus-summary-article-header article)))
             (raw     (and header (mail-header-id header)))
             (mid     (and raw (gnaw-normalize-mid raw))))
        (when mid (funcall fn article mid)))
      (forward-line 1))))

(defun gnus-gnaw--build-mid-map (reports)
  "Build a hash of normalized MIDs to report info for REPORTS."
  (let ((id-map (make-hash-table :test 'equal)))
    (dolist (r reports)
      (puthash (car r) (cdr r) id-map))
    id-map))

(defun gnus-gnaw--apply-overlays (reports)
  "Apply overlays for BONE REPORTS in the current summary buffer."
  (remove-overlays (point-min) (point-max) 'gnus-gnaw t)
  (let ((id-map (gnus-gnaw--build-mid-map reports))
        (state  (gnaw-read-state)))
    (gnus-gnaw--for-each-summary-mid
     (lambda (_article mid)
       (let ((info (gethash mid id-map)))
         (when info
           (let* ((entry   (cdr (assoc mid state)))
                  (bol     (line-beginning-position))
                  (eol     (line-end-position))
                  (ann-str (gnaw-annotation info entry))
                  (ann-len (length ann-str))
                  (top     (equal "A" (gnaw-priority-letter
                                       (plist-get info :priority))))
                  (face    (if top '(gnus-gnaw-face bold) 'gnus-gnaw-face))
                  (ov-bg   (make-overlay bol eol))
                  (tag-len (+ ann-len 1))
                  (start   (max bol (- eol tag-len)))
                  (ov-ann  (make-overlay start eol)))
             (overlay-put ov-bg 'face face)
             (overlay-put ov-bg 'gnus-gnaw t)
             (overlay-put ov-ann 'display
                          (propertize (concat " " ann-str)
                                      'face 'gnus-gnaw-annotation-face))
             (overlay-put ov-ann 'gnus-gnaw t))))))))

(defvar-local gnus-gnaw--active-reports nil
  "Buffer-local cache of BONE reports for auto-rehighlighting.")

(defvar-local gnus-gnaw--active-topic nil
  "Topic filtering the buffer's reports, or nil for all reports.")

(defun gnus-gnaw--rehighlight (&rest _args)
  "Re-apply overlays on summary buffer updates."
  (when gnus-gnaw--active-reports
    (gnus-gnaw--apply-overlays gnus-gnaw--active-reports)))

(defun gnus-gnaw--enable-hooks ()
  "Enable hooks in current summary buffer."
  (add-hook 'gnus-summary-prepared-hook #'gnus-gnaw--rehighlight nil t)
  (add-hook 'gnus-summary-update-hook #'gnus-gnaw--rehighlight nil t))

(defun gnus-gnaw--disable-hooks ()
  "Disable hooks in current summary buffer."
  (remove-hook 'gnus-summary-prepared-hook #'gnus-gnaw--rehighlight t)
  (remove-hook 'gnus-summary-update-hook #'gnus-gnaw--rehighlight t))

(defun gnus-gnaw--activate (reports &optional limit-articles topic)
  "Activate REPORTS, optionally limiting summary to LIMIT-ARTICLES first.
TOPIC records the topic filter producing REPORTS, if any."
  (when limit-articles (gnus-summary-limit limit-articles))
  (gnus-gnaw-clear)
  (setq gnus-gnaw--active-reports reports
        gnus-gnaw--active-topic topic)
  (gnus-gnaw--apply-overlays reports)
  (gnus-gnaw--enable-hooks))

;;; Commands

;;;###autoload
(defun gnus-gnaw-highlight ()
  "Highlight summary lines of open BONE reports."
  (interactive)
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "Not in a Gnus summary buffer"))
  (let ((reports (gnaw-reports)))
    (if (null reports)
        (message "No open BONE reports found.")
      (gnus-gnaw--activate reports)
      (message "Highlighted %d BONE reports." (length reports)))))

(defun gnus-gnaw--matching-articles (reports)
  "Get article numbers matching REPORTS."
  (let ((id-map (gnus-gnaw--build-mid-map reports))
        (articles nil))
    (gnus-gnaw--for-each-summary-mid
     (lambda (article mid)
       (when (gethash mid id-map)
         (push article articles))))
    (nreverse articles)))

;;;###autoload
(defun gnus-gnaw ()
  "Limit summary to open BONE reports and highlight them."
  (interactive)
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "Not in a Gnus summary buffer"))
  (let ((reports (gnaw-reports)))
    (if (null reports)
        (message "No open BONE reports found.")
      (let ((articles (gnus-gnaw--matching-articles reports)))
        (if (null articles)
            (message "No matching articles in this summary.")
          (gnus-gnaw--activate reports articles)
          (message "Limited to %d BONE reports." (length articles)))))))

;;;###autoload
(defun gnus-gnaw-topic ()
  "Limit summary to reports of selected topic and highlight them."
  (interactive)
  (unless (derived-mode-p 'gnus-summary-mode)
    (user-error "Not in a Gnus summary buffer"))
  (let* ((reports (gnaw-reports))
         (topics  (gnaw-topics reports)))
    (cond
     ((null reports) (message "No open BONE reports found."))
     ((null topics)  (message "No topics in any report."))
     (t
      (let* ((topic    (completing-read "BONE topic: " topics nil t))
             (filtered (and (not (string= topic ""))
                            (gnaw-filter-by-topic reports topic))))
        (cond
         ((or (string= topic "") (null filtered))
          (message "No reports for topic \"%s\"." topic))
         (t
          (let ((articles (gnus-gnaw--matching-articles filtered)))
            (if (null articles)
                (message "No matching articles for topic \"%s\"." topic)
              (gnus-gnaw--activate filtered articles topic)
              (message "Limited to %d BONE reports for topic \"%s\"."
                       (length articles) topic))))))))))

;;; Marking commands

(defun gnus-gnaw--current-mid ()
  "Current article's normalized MID, or nil."
  (let ((article (gnus-summary-article-number)))
    (when (and (numberp article) (> article 0))
      (let* ((header (gnus-summary-article-header article))
             (raw    (and header (mail-header-id header))))
        (when raw
          (gnaw-normalize-mid raw))))))

(defun gnus-gnaw--info-for-mid (mid reports)
  "Return info plist for MID in REPORTS."
  (cdr (assoc mid reports)))

(defun gnus-gnaw--mark (action on-msg off-msg)
  "Toggle ACTION mark on the current report, showing ON-MSG or OFF-MSG."
  (let* ((reports (or gnus-gnaw--active-reports (gnaw-reports)))
         (mid     (and reports (gnus-gnaw--current-mid)))
         (info    (and mid (gnus-gnaw--info-for-mid mid reports))))
    (cond
     ((null reports) (user-error "No BONE reports loaded"))
     ((null mid)     (user-error "No message-id on current line"))
     ((null info)    (user-error "Current article is not a BONE report: %s" mid))
     (t
      (let ((on (gnaw-toggle-mark mid info action)))
        (gnus-gnaw--rehighlight)
        (message "%s" (if on on-msg off-msg)))))))

;;;###autoload
(defun gnus-gnaw-mark-sticky ()
  "Toggle the sticky mark (keep visible) for the current report."
  (interactive)
  (gnus-gnaw--mark :sticky "Marked sticky" "Unmarked sticky"))

;;;###autoload
(defun gnus-gnaw-mark-dismiss ()
  "Toggle the dismiss mark (hide) for the current report."
  (interactive)
  (gnus-gnaw--mark :dismiss "Dismissed" "Undismissed"))

;;;###autoload
(defun gnus-gnaw-clear ()
  "Remove all gnus-gnaw overlays."
  (interactive)
  (remove-overlays (point-min) (point-max) 'gnus-gnaw t)
  (setq gnus-gnaw--active-reports nil
        gnus-gnaw--active-topic nil)
  (gnus-gnaw--disable-hooks))

;; --- Cache update hooks ----------------------------------------------------

(defun gnus-gnaw--refresh-all-buffers ()
  "Re-apply overlays in active summary buffers from the refreshed cache.
A buffer set up by `gnus-gnaw-topic' keeps its topic filter."
  (let ((reports (gnaw-reports)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'gnus-summary-mode)
                   gnus-gnaw--active-reports)
          (setq gnus-gnaw--active-reports
                (if gnus-gnaw--active-topic
                    (gnaw-filter-by-topic reports gnus-gnaw--active-topic)
                  reports))
          (gnus-gnaw--apply-overlays gnus-gnaw--active-reports))))))

(add-hook 'gnaw-after-update-hook #'gnus-gnaw--refresh-all-buffers)

(provide 'gnus-gnaw)
;;; gnus-gnaw.el ends here
