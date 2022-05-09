(add-hook 'rust-mode-hook
          (lambda ()
            (setq rust-format-on-save t
                  rust-rustfmt-bin "~/.cargo/bin/rustfmt")))
            
(provide 'init-rust)
