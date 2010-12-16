
(add-path "site-lisp/coffee-mode")

(autoload 'js2-mode "js2-mode" nil t)
(autoload 'coffee-mode "coffee-mode" "Coffee mode" t)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

(setq js-indent-level 2
      js2-basic-offset 2
      js2-clean-up-whitespace t
      js2-strict-missing-semi-warning nil)

(defun mine-js2-indent-function ()
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status))
           node)

      (save-excursion
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ js-indent-level 2))))

        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun mine-js2-mode-hook ()
  (if (not (boundp 'js--proper-indentation))
      (progn (js-mode)
             (remove-hook 'js2-mode-hook 'mine-js2-mode-hook)
             (js2-mode)
             (add-hook 'js2-mode-hook 'mine-js2-mode-hook)))
  (set (make-local-variable 'indent-line-function) 'mine-js2-indent-function)
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (define-key js2-mode-map [(backspace)] 'c-electric-backspace)
  (define-key js2-mode-map [(control d)] 'c-electric-delete-forward))

(add-hook 'js2-mode-hook 'mine-js2-mode-hook)

(provide 'mine-javascript)