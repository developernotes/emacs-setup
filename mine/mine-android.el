(defvar proguard-bin-path "~/bin/android-sdk/tools/proguard/bin")
(defvar android-retrace-buffer "*android-retrace*")

(defun android-retrace ()
  "Convert an obfuscated stack trace to a readable one.
Run within a stacktrace file that exists in the same
directory as the ProGuard mapping file."
  (interactive)
  (when (get-buffer android-retrace-buffer)
    (kill-buffer android-retrace-buffer))
  (let* ((results (shell-command-to-string (format "%s %s %s"
                                                   (format "%s/%s" proguard-bin-path "retrace.sh")
                                                   (format "%s%s" default-directory "mapping.txt")
                                                   (buffer-file-name (current-buffer)))))
         (results-buffer (get-buffer-create android-retrace-buffer)))
         (display-buffer results-buffer)
         (switch-to-buffer-other-window results-buffer)
         (java-mode)
         (local-set-key (kbd "q") 'quit-window)
         (insert results)
         (beginning-of-buffer)))

(provide 'mine-android)
