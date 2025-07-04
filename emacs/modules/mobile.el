;; Useful during iOS penetration tests. The shell in jailbroken
;; devices is not in /bin/sh, so we must tell explicitly tramp where
;; to look for.

;; NOTE: if there are problems with TRAMP, considering cleaning the
;; cache with 'tramp-cleanup-all-connections'.

(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/ssh:mobile-ios:")
                   "remote-shell" "/var/jb/usr/bin/zsh"))

(add-to-list 'tramp-connection-properties
             (list (regexp-quote "/scp:mobile-ios:")
                   "remote-shell" "/var/jb/usr/bin/zsh"))

(add-to-list 'tramp-remote-path "/var/jb/usr/bin/" "/var/jb/bin/")
