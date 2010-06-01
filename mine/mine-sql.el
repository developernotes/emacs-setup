

(defun mine-mysql (user password host database)
  (let ((sql-user user)
        (sql-password password)
        (sql-server host)
        (sql-database database))
    (call-interactively 'sql-mysql)
    (rename-buffer (concat "*SQL*:" database ":" host))))


(provide 'mine-sql)