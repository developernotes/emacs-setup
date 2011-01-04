(require 'powershell)

(autoload 'powershell-mode "powershell-mode" "A editing mode for Microsoft PowerShell." t)

(add-to-list 'auto-mode-alist '("\\.ps1$" . powershell-mode))

(defvar powershell-inferior-process-buffername "*PowerShell*")

(add-hook 'powershell-mode-hook '(lambda ()
                                   (setq powershell-indent 2)
                                   (local-set-key (kbd "C-c i") 'powershell)
                                   (local-set-key (kbd "C-c b") 'powershell-eval-buffer)
                                   (local-set-key (kbd "C-c r") 'powershell-eval-region)))

(defun powershell-eval-buffer ()
  "Evaluates the current buffer inside a PowerShell inferior process"
  (interactive)
  (powershell-eval-region (point-min) (point-max)))

(defun powershell-eval-region (start end)
  "Send current region to PowerShell inferior process"
  (interactive "r")
  (comint-send-region powershell-inferior-process-buffername start end)
  (comint-send-string powershell-inferior-process-buffername "\n"))

(provide 'mine-powershell)