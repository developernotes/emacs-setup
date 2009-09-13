(setq erc-nick "developernotes")
(setq erc-prompt-for-password t)
(setq erc-server "irc.freenode.net")
(setq erc-user-full-name "Nick Parker")
 
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#git")))
 
(setq erc-interpret-mirc-color t)
 
(setq erc-modules
      '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track))
 
(provide 'mine-erc)
