(setq erc-nick "developernotes"
      erc-prompt-for-password t
      erc-server "irc.freenode.net"
      erc-user-full-name "Nick Parker"
      erc-interpret-mirc-color t
      erc-autojoin-channels-alist '(("freenode.net" "#emacs" "#git"))
      erc-modules
      '(autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring services stamp track))

(provide 'init-erc)
