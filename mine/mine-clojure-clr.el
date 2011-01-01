
(defvar clojure-clr-repl-bin "Clojure.Main.exe")
(defconst clojure-clr-buffer-name "*Clojure-CLR*")

(defun clojure-clr-run-interpreter ()
  "Launches a Clojure CLR interpreter"
  (interactive)
  (when (get-buffer clojure-clr-buffer-name)
    (kill-buffer clojure-clr-buffer-name))
    (apply 'make-comint "Clojure-CLR" clojure-clr-repl-bin nil nil)
    (display-buffer clojure-clr-buffer-name))

(defun clojure-clr-eval-buffer ()
  "Evaluates the current buffer inside a Clojure-CLR interpreter"
  (interactive)
  (clojure-clr-eval-region (point-min) (point-max)))

(defun clojure-clr-eval-region (start end)
  "Send current region to Clojure-CLR interpreter."
  (interactive "r")
  (comint-send-region clojure-clr-buffer-name start end)
  (comint-send-string clojure-clr-buffer-name "\n"))

(add-hook 'clojure-mode-hook '(lambda ()
                                (define-key clojure-mode-map (kbd "C-c i") 'clojure-clr-run-interpreter)
                                (define-key clojure-mode-map (kbd "C-c b") 'clojure-clr-eval-buffer)
                                (define-key clojure-mode-map (kbd "C-c r") 'clojure-clr-eval-region)))

(provide 'mine-clojure-clr)