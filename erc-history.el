;;; erc-log.el --- An Emacs Internet Relay Chat Log  -*- lexical-binding:t -*-

;; Copyright (C) 2024 Free Software Foundation, Inc.

;; Author: Ebeem <ebeem2@gmail.net>
;; Maintainer: Ebeem <ebeem2@gmail.net>
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (compat "29.1.3.4") (erc "5.5.0"))
;; Keywords: IRC, chat, client, Internet
;; URL: https://www.gnu.org/software/emacs/erc.html

;;; Commentary:

;; The code contained in this module is responsible for pulling
;; chat logs into ERC buffers.  In order to actually activate this,
;; you must call `erc-history-mode'.

;; You must also customize `erc-history-sources' to list the channel
;; logs available along with their sources/hosts.

;; example of some logs
;; 2024-06-10 08:00:01 - ByteMaster: Good morning, team! Ready for a productive week?
;; 2024-06-10 08:01:05 - CodeWarrior: Morning ByteMaster! Excited to start on the new API.
;; 2024-06-10 08:02:12 - Hackerman: Hey everyone! Any issues from the weekend?
;; 2024-06-10 08:03:21 - NetNinja: Just a minor bug, already resolved.
;; 2024-06-10 08:04:35 - CyberSage: Morning! I’ll be working on system security today.

;;; Code:

(require 'erc)
(require 'url)
(require 'cl-lib)

(defvar erc-history-sources
  '(("http://localhost/grc-history/#CHANNEL#/%Y/%m/%d.txt"
     ("#erc-history" "#erc-history-1")))
  "Lists of sources to pull history from and the channels they provide.")

(defvar erc-history-pull-on-reaching-top t
  "Whether to automatically pull messages logs upon reaching top of a buffer.")

(defvar erc-history-time-offset
  (car (current-time-zone (current-time)))
  "The offset from UTC, will be added to each timestamp.")

(defvar-local erc-history-last-pulled-date (current-time)
  "Last date pulled into the current buffer.")

(defvar-local erc-history--source-available nil
  "Whether a source if available for this channel.")

;;;###autoload(autoload 'erc-history-mode "erc-history" nil t)
(define-erc-module history history
  "This mode allows loading previous history to current buffer."
  ((erc-history--enable))
  ((erc-history--disable)))

(defun erc-history--enable ()
  "Init the erc-history module locally.
It will ensure that the current buffer is defined in the sources list."
  (setq erc-history--source-available (erc-history--get-channel-url))
  (when (and erc-history-pull-on-reaching-top
             erc-history--source-available)
    (add-hook 'post-command-hook 'erc-history--check-point-at-top-of-buffer nil t)))
   

(defun erc-history--disable ()
  "Init the erc-history module locally.
It will ensure that the current buffer is defined in the sources list."
  (remove-hook 'post-command-hook 'erc-history--check-point-at-top-of-buffer t))

(defun erc-history--get-channel-url (&optional channel)
  "Return the base url of the host that archives the irc logs for CHANNEL."
  (cl-loop for source in erc-history-sources
           when (member (or channel
                            (buffer-name (current-buffer)))
                        (nth 1 source))
           return (car source)))

(defun erc-history--get-target-chat-url (&optional url date channel)
  "Return a crafted url of the target chat log after replacing all placeholders.
URL is the base url of the host that archives the logs (default is sources)
DATE must be passed to get the specific date history (default is last date)
CHANNEL is the name of the irc channel (default is buffer name)"
  (string-replace "#CHANNEL#"
                  ;; encoding only the channel name
                  (string-replace "#" "%23" (or channel (buffer-name (current-buffer))))
                  (format-time-string (or url erc-history--source-available "")
                                      (or date erc-history-last-pulled-date))))

(defun erc-history-decrement-date (&optional days)
  "Subtract a number of DAYS from the erc-history-last-pulled-date.
erc-history-last-pulled-date is the used variable to decide which chat logs to
pull next."
  (setq erc-history-last-pulled-date
        (time-subtract erc-history-last-pulled-date (days-to-time (or days 1))))
  erc-history-last-pulled-date)

(message "%s" (decode-time
               (current-time)))

(message "%s" (parse-time-string (concat (substring "2024-06-10T08:00:01 -" 0 19) "+0000")))
(message "%s" (decode-time
               (encode-time (parse-time-string (substring "2024-06-10 08:00:01 -" 0 19)))))

(message "%s" (time-add (parse-time-string (substring "2024-06-10 08:00:01 -" 0 19))
                        erc-history-time-offset))

(defun erc-history-display-line (channel msg)
  "Display MSG in its erc bubber named CHANNEL.
MSG must match the format described for erc messages."
  ;; TODO: include parsing function in source configs
  (let ((msg-parts (string-split msg " ")))
    (when (> (length msg-parts) 3)
      (let ((nickname (substring (nth 3 msg-parts) 0
                                 (- (length (nth 3 msg-parts)) 1)))
            (content (mapconcat 'identity (cl-subseq msg-parts 4) " "))
            (time (encode-time (parse-time-string
                                (concat
                                 (string-replace " " "T" (substring msg 0 19))
                                 ;; TODO: include time zone parameter in source configs
                                 "+0000")))))
        (with-current-time time
                           (lambda ()
                             (with-current-buffer channel
                               (set-marker erc-insert-marker (point-min))
                               (erc-display-line
                                (concat "<" nickname "> " content)
                                (erc-get-buffer channel))
                               (set-marker erc-insert-marker (point-max)))))))))

(defun with-current-time (time body)
  "Execute BODY with the current time temporarily set to TIME."
  (let ((orig-current-time (symbol-function 'current-time)))
    (cl-letf (((symbol-function 'current-time) (lambda () time)))
      (unwind-protect
          (funcall body)
        (fset 'current-time orig-current-time)))))

(defun erc-history-pull-previous (&optional channel)
  "Load previous history of CHANNEL."
  (interactive "e")
  (let ((channel (or channel (buffer-name (current-buffer)))))
    (message "loading history of %s at %s from: %s" channel
             (format-time-string "%Y-%m-%d" erc-history-last-pulled-date)
             (erc-history--get-target-chat-url))
    
    (url-retrieve (erc-history--get-target-chat-url)
                  (lambda (status)
                    (erc-history--url-callback status channel)))

    (let* ((decoded (decode-time erc-history-last-pulled-date))
           (year (nth 5 decoded))
           (month (nth 4 decoded))
           (day (nth 3 decoded)))
      (with-current-time (encode-time 0 0 0 day month year)
                        (lambda ()
                          (with-current-buffer channel
                            (set-marker erc-insert-marker (point-min))
                            (erc-display-line "" (erc-get-buffer channel))
                            (set-marker erc-insert-marker (point-max)))))
    (erc-history-decrement-date))))

(defun erc-history--check-point-at-top-of-buffer ()
  "Call pull history if point reached top."
  (when (bobp)
    (erc-history-pull-previous)))

(defun erc-history--url-callback (status channel)
  "Append messages to display of CHANNEL after fetching them from url.
displays an error message if STATUS is error."
  (if (plist-get status :error)
      (message "Error retrieving URL: %s" (plist-get status :error))
    (goto-char (point-min))
    (search-forward-regexp "\n\n" (point-max) t)
    (let ((min-http-point (point)))
      (goto-char (point-max))
      (while (> (point) min-http-point)
        (forward-line -1)
        (when (> (- (line-end-position)
                    (line-beginning-position) 0))
          (erc-history-display-line channel (buffer-substring-no-properties
                                            (line-beginning-position)
                                            (line-end-position)))))))
  (kill-buffer (current-buffer)))

;; TODO: create a function that helps at getting the marker to the correct
;; line based on timestamp

;; (defun get-min-max-stamps ()
;;   (let ((stamp-min nil)
;;         (stamp-max nil))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (let (words)
;;         (while (not (eobp))
;;           ;; (message "%s" (text-properties-at (point)))
;;           (let ((stamp (get-text-property (point) 'invisible))
;;                 (stamp-fun (car (get-text-property (point) 'cursor-sensor-functions)))
;;                 (and (equal stamp 'timestamp) (decode-time (aref (aref stamp-fun 2) 0))))
;;             (when (equal stamp 'timestamp)
;;               (when (or stamp-min) (setq stamp-min stamp)))
;;           (forward-word))))))
;; (with-current-buffer "#erc-history"
;;   (goto-char (point-min))
;;   (let (words)
;;     (while (not (eobp))
;;       ;; (message "%s" (text-properties-at (point)))
;;       (let ((stamp (get-text-property (point) 'invisible))
;;             (stamp-fun (car (get-text-property (point) 'cursor-sensor-functions))))
;;         (when (equal stamp 'timestamp)
;;           (message "stamp found! %s" (decode-time (aref (aref stamp-fun 2) 0)))))
;;       (forward-word))))

(provide 'erc-history)
;;; erc-history.el ends here
