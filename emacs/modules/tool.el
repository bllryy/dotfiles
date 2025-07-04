;;; tool.el --- Useful Abstraction -*- lexical-binding: t -*-
;;
;; Author: Leonardo Tamiano
;;
;; ------------------------------------------------------------------------

;; ------------------
;; *GENERAL*

;; TODO: add ability to rename buffer the generic async-shell-buffer
;; used to create the output command
(defun my/tool-execute-as-root (command &optional buffer-name)
  (interactive "Execute as root: ")
  (let* ((async-shell-command-buffer 'new-buffer)
         )
    (with-temp-buffer
      (cd "/sudo::/")
      (async-shell-command command))
    )
  )

(defun my/tool-execute-cmd (&optional cmd)
  "Wrapper on the tool script"
  (interactive)
  (if cmd
      (let* ((async-shell-command-buffer 'new-buffer))
        (shell-command cmd)
        )
    (let* ((cmd-input (read-from-minibuffer "tool: "))
           (full-cmd (concat "tool " cmd-input))
           )
      (shell-command-to-string "tool whoami")
      )
    )
  )

(defun my/tool-async ()
  "Wrapper on the tool script"
  (interactive)
  (async-shell-command (concat "tool " (read-from-minibuffer "tool: "))))

;; ------------------
;; *TOOL*

(defun my/tool ()
  (interactive)
  ;; depending on the enviroment I have different tools
  (cond
   ((string-match-p (regexp-quote "pwndoc") (buffer-name))
    (pwndoc-ui))

   (env/work (my/tool-work))
   (env/home (my/tool-home))
   )
  )

(setq my/tool-core
      [:class transient-row "Core\n"
              ("1" "WAPT" my/tool-wapt)
              ("2" "MAPT" my/tool-mapt)
              ("3" "NPT" my/tool-npt)
              ("4" "SCR" my/tool-scr)
              ("5" "Report" my/tool-report)
              ])

(setq my/tool-utils
      [:class transient-row "Utils \n"
              ("a" "Agenda" my/tool-agenda)
              ("p" "Password" pm-ui)
              ("v" "VM" my/tool-vm)
              ("l" "Layout" my/tool-layout)	      
              ("n" "Network" my/tool-misc-network)
              ("b" "Backup" my/tool-backup)
              ("d" "Encoder" my/tool-encoder)	      
              ("y" "YouTube" yt-ui :if (lambda () env/home))
              ]
      )

(setq my/tool-system-management-ui
      [:class transient-row "System\n"
              ("q" "quit" keyboard-quit)
              ]
      )

(transient-define-prefix my/tool-work ()
  [:class transient-row "Activity\n"
          ("-" (lambda () (interactive))
           :description
           (lambda () (interactive)
             (concat (if (not my/work-current-activity) "" my/work-current-activity) "\n\n")
             ))
          ("e0" "select" my/tool-work-activity-switch)	  
          ("e1" "dir" my/tool-work-activity-dir)
          ("e2" "shell" my/tool-work-activity-shell)
          ("e3" "data" my/tool-work-activity-data)
          ("e4" "new" my/tool-work-activity-new)
          ("e5" "rm" my/tool-work-activity-rm)
          ]

  my/tool-core
  my/tool-utils
  my/tool-system-management-ui
  )

(transient-define-prefix my/tool-home ()
  my/tool-core
  my/tool-utils
  my/tool-system-management-ui
  )

;; ------------------
;; *ACTIVITY*

(defun my/tool-work-activitity-list ()
  (sort
   (delq nil
         (mapcar (lambda (entry)
                   (when (string-match-p "MINDED/activity" entry)
		     (nth 4 (split-string entry "/")))
                   ) projectile-known-projects)
	 )
   (lambda (x y)
     (let* ((x-split (split-string x "-"))
	    (y-split (split-string y "-"))
	    (x-date (concat (nth 1 x-split) (nth 2 x-split)))
	    (y-date (concat (nth 1 y-split) (nth 2 y-split)))
	    )
       (string> x-date y-date)
       )
     )
   )
  )

(defun my/tool-work-activity-switch ()
  (interactive)
  (let* ((ivy-sort-functions-alist nil)
         (selected-activity (ivy-read "Activity: " (cons "NONE" (my/tool-work-activitity-list))))
         )

    (if (string-equal selected-activity "NONE")
	(setq my/work-current-activity nil
	      my/work-current-activity-dir nil
	      my/work-current-activity-data nil)
      (setq 
       my/work-current-activity selected-activity
       
       my/work-current-activity-dir
       (format "/home/leo/projects/MINDED/activity/%s" selected-activity)
       
       my/work-current-activity-data
       (condition-case err
	   (read (with-temp-buffer
		   (insert-file-contents (concat my/work-current-activity-dir "/.data"))
		   (buffer-string)))
	 (error
	  (message "Error reading file: %s" (error-message-string err))
	  nil)))
      )
    )
  )

(setq work-activity-types
      '(WAPT MAPT SCR TRAINING GUIDELINE RETEST RETEST-WAPT RETEST-MAPT RETEST-SCR SAMM))
      
(defun my/tool-work-activity-new ()
  (interactive)
  ;; TODO:
  ;; - create name of activity using an interactive process:
  ;;    - first, ask for type of activity
  ;;    - then, compute current year and month
  ;;    - finally, ask for name of activity
  ;; - create basic folder in /home/leo/projects/MINDED/activities
  ;; - folder has to have basic structure
  ;; - file named 'README.org' with title, author and date, and org outlines 'abstract', 'issues', 'notes', 'misc', 'refs'
  ;; - the created folders depend on the type of activity
  ;; - for WAPT create directories: st, report, notes, burpsuite, code
  (let* ((ivy-sort-functions-alist nil)
         (act-type (read (ivy-read "Type: " work-activity-types)))
         (date (format-time-string "%Y-%m"))
         (client-name (ivy-read "Client Name: " nil))
         (app-name (ivy-read "App Name: " nil))
         (activity-name (format "%s-%s-%s-%s" act-type date client-name app-name))
         (activity-dir (format "/home/leo/projects/MINDED/activity/%s" activity-name))
         )
    (make-directory activity-dir t)

    ;; initialize README.org file
    (with-temp-buffer
      (org-mode)
      (insert (format "#+TITLE: %s\n" activity-name))
      (insert (format "#+AUTHOR: Leonardo Tamiano\n"))
      (insert (format "#+DATE: %s\n" (format-time-string "[%Y-%m-%d %a %H-%M]")))
      (insert (format "\n"))
      (insert (format "* Abstract\n"))
      (insert (format "* TODOs\n"))
      (insert (format "* Issues\n"))
      (insert (format "* Notes\n"))
      (insert (format "* Misc\n"))
      (insert (format "* Refs\n"))

      (write-file (format "%s/README.org" activity-dir))
      )

    ;; create empty .projectile
    ;; TODO: refresh projectile
    (with-temp-buffer
      (write-file (format "%s/.projectile" activity-dir))
      )

    ;; initialize folder structure depending on type of activity
    ;; TODO: finalize
    (cond
     ((eq act-type 'WAPT) (message "WAPT!"))
     ((eq act-type 'SCR) (message "SCR!"))
     )

    ;; make sure projectile.el will read the new project for quick access in the future
    ;; (projectile-invalidate-cache)
    (projectile-discover-projects-in-search-path)
    )
  )

(defun my/tool-work-activity-shell ()
  (interactive)
  (let* ((act-shell-buffer (get-buffer "act"))
         )
    (when act-shell-buffer
      (kill-buffer act-shell-buffer))
    (setq-local default-directory my/work-current-activity-dir)
    (vterm "act")
    )
  )

(defun my/tool-work-activity-data ()
  "Extract data of interest related to the selected activity."
  (interactive)

  (defun my/tool-get-credential (data)
    (let* ((username (ivy-read "User: " (assoc-default 'credentials data)))
	   (type (ivy-read "Value: " '("Username" "Password")))
	   (returned-value (if (string-equal type "Username")
			       username
			     (assoc-default username (assoc-default 'credentials data))))
	   )
      returned-value
      )
    )  

  (let* ((ivy-sort-functions-alist nil)
	 (key (ivy-read "Data: "
			(mapcar (lambda (entry) (car entry))
				my/work-current-activity-data)))
	 (value (cond ((string-equal key "scope")
		       (ivy-read "Scope: " (alist-get 'scope my/work-current-activity-data)))
		      ((string-equal key "credentials")
		       (my/tool-get-credential my/work-current-activity-data))
		      (t
		       (alist-get (read key) my/work-current-activity-data))
		       ))
	 )
    (kill-new value)
    )
  )

(defun my/tool-work-activity-dir ()
  (interactive)
  (dired my/work-current-activity-dir)
  )

(defun my/tool-work-activity-rm ()
  (interactive)
  ;; TODO: implement me
  ;; (projectile-remove-known-project "~/projects/MINDED/activity/WAPT-2024-11-TEST-TEST/")
  )

;; ------------------
;; *WAPT*

(transient-define-prefix my/tool-wapt ()
  [:class transient-row "Tool -> WAPT \n"
          ("e1" "Browser" my/tool-browser-start)
          ("e2" "WAPT.el" wapt-start)
          ("e3" "Burpsuite" (lambda () (interactive) (my/tool-execute-cmd "tool burp")))
          ]

  my/tool-system-management-ui
  )

(defun my/tool-browser-profile-list ()
  (if env/work
      '("work" "proxy" "personal")
    '("proxy" "live")))

(defun my/tool-browser-start ()
  (interactive)
  (let* ((ivy-sort-functions-alist nil)
         (namespace-list (append '("host") (my/tool-misc-network-namespace-list)))
         (namespace (if (eq (length namespace-list) 1)
                        (nth 0 namespace-list)
                      (ivy-read "Namespace: " namespace-list)))
         (is-host? (string-equal namespace "host"))
         (browser-profile (ivy-read "Profile: " (my/tool-browser-profile-list) ))
         (namespace-cmd (my/tool-misc-network-namespace-cmd-format namespace))
         (profile-cmd (my/tool-browser-profile namespace browser-profile))
         (cmd (format namespace-cmd
                      (if (not is-host?)
                          (if env/work
                              (concat
                               "sudo mount -t cgroup2 cgroup2 /sys/fs/cgroup" " && "
                               "sudo mount -t securityfs securityfs /sys/kernel/security/ " " && "
                               (format "sudo -u leo chromium %s" profile-cmd)
                               ">/dev/null 2>&1 & "
                               )
                            (format "sudo -u leo chromium %s >/dev/null 2>&1 & " profile-cmd)
                            )
                          (format "chromium %s >/dev/null 2>&1 & " profile-cmd)
                        )
                      )
              )
         )
    (if is-host?
        (my/tool-execute-cmd cmd)
      (my/tool-execute-as-root cmd)
      )
    )
  )

(defun my/tool-browser-profile (namespace browser-profile)
  (concat
   (format "--user-data-dir=/home/leo/tool/chromium/%s-%s " namespace browser-profile)
   (cond
    ((string-equal browser-profile "work")
     ""
     )
    ((string-equal browser-profile "proxy")
     (concat
      "--proxy-server=127.0.0.1:8080 "
      "--ignore-certificate-errors "
      "--proxy-bypass-list='<-loopback>' "
      ))
    ((string-equal browser-profile "live")
     "--enable-features=WebContentsForceDark "
     )
    )
   )
  )

;; ------------------
;; *MAPT*

(transient-define-prefix my/tool-mapt ()
  [:class transient-row "Tool -> MAPT \n"
          ("1" "Android" my/tool-mapt-android)
          ("2" "iOS" my/tool-mapt-ios)
          ]
  my/tool-system-management-ui
  )

;; ------------------
;; *MAPT->ANDROID*

(transient-define-prefix my/tool-mapt-android ()
  [:class transient-row "Tool -> MAPT -> Android \n"
          ("1" "Emulator" my/tool-mapt-android-emulator)
          ("2" "APK" my/tool-mapt-android-apk)
          ("3" "ADB" my/tool-mapt-android-adb)
          ]
  )

;; -------------------------
;; *MAPT->ANDROID->EMULATOR*

(transient-define-prefix my/tool-mapt-android-emulator ()
  [:class transient-row "Tool -> MAPT -> Android -> Emulator \n"
          ("1" "Start" my/tool-mapt-android-emulator-start)
          ("2" "Stop" my/tool-mapt-android-emulator-stop)
          ("3" "Install" my/tool-mapt-android-emulator-install)
          ("4" "Remove" my/tool-mapt-android-emulator-remove)
          ]
  )


(defun my/tool-mapt-android-emulator-start ()
  (interactive)
  (let* ((android-root "/home/leo/tool/android/")
         (avds (split-string (shell-command-to-string (concat android-root "cmdline-tools/latest/bin/avdmanager list avd -c")) "\n"))
         (choice (ivy-read "Device to start: " avds))
         (cmd (format "%s/emulator/emulator -avd %s -scale 0.6 -writable-system >/dev/null 2>&1 &" android-root choice))
         )
    (with-environment-variables (("ANDROID_AVD_HOME" "/home/leo/.config/.android/avd"))
      (shell-command cmd)
      )
    )
  )

(defun my/tool-mapt-android-emulator-stop ()
  (interactive)
  (let* ((cmd (format "/home/leo/tool/android/platform-tools/adb emu kill"))
         )
    (with-environment-variables (("ANDROID_AVD_HOME" "/home/leo/.config/.android/avd"))
      (shell-command cmd)
      )
    )
  )

(defun my/tool-mapt-android-emulator-install ()
  (interactive)
  (lambda () (interactive) (message "TODO: implement me "))
  )

(defun my/tool-mapt-android-emulator-remove ()
  (interactive)
  (lambda () (interactive) (message "TODO: implement me "))
  )

;; -------------------------
;; *MAPT->ANDROID->APK*

(transient-define-prefix my/tool-mapt-android-apk ()
  [:class transient-row "Tool -> MAPT -> Android -> APK \n"
          ("1" "Install" my/tool-mapt-android-apk-install)
          ("2" "Remove" (lambda () (interactive) (message "TODO: implement me ")))
          ("3" "Version" my/tool-mapt-android-apk-version)
          ]
  )

(defun my/tool-mapt-android-apk-install ()
  (interactive)
  (let* ((apk (nth 0 (find-file-read-args "Select apk: " t)))
         (cmd (format "/home/leo/tool/android/platform-tools/adb install %s" apk))
         )
    (with-environment-variables (("ANDROID_AVD_HOME" "/home/leo/.config/.android/avd"))
      (shell-command cmd)
      )
    )
  )

(defun my/tool-mapt-android-apk-version ()
  (interactive)
  (let* ((apk (nth 0 (find-file-read-args "Select apk: " t)))
         (aapt "/home/leo/tool/android/build-tools-34-rc2/aapt ")
         (version-name-cmd (format "%s dump badging %s | grep -o 'versionName=[^,]*' | cut -d'=' -f 2 | cut -d ' ' -f 1 | tr -d \\'" aapt apk))
         (version-code-cmd (format "%s dump badging %s | grep -o 'versionCode=[^,]*' | cut -d'=' -f 2 | cut -d ' ' -f 1 | tr -d \\'" aapt apk))
         (md5sum-cmd (format "md5sum %s | cut -d ' ' -f1" apk))
         (version-name (my/cmd-to-string version-name-cmd))
         (version-code (my/cmd-to-string version-code-cmd))
         (md5sum (my/cmd-to-string md5sum-cmd))
         (output (format
                  "file_path    = %s
version_name = %s
version_code = %s
md5checksum  = %s"
                  apk
                  version-name
                  version-code
                  md5sum)
                 )
         )

    (kill-new output)
    )
  )

;; -------------------------
;; *MAPT->ANDROID->ADB*

(transient-define-prefix my/tool-mapt-android-adb ()
  [:class transient-row "Tool -> MAPT -> Android -> ADB \n"
          ("1" "General" my/tool-mapt-android-adb-general)
          ("2" "Shell" my/tool-mapt-android-adb-shell)
          ("3" "Proxy" my/tool-mapt-android-adb-proxy)
          ]
  )

(defun my/tool-mapt-android-adb-general ()
  (interactive)
  (lambda () (interactive) (message "TODO: implement me"))
  )

(defun my/tool-mapt-android-adb-shell ()
  (interactive)
  (let* ((cmd (format "/home/leo/tool/android/platform-tools/adb shell" ))
         )
    (with-environment-variables (("ANDROID_AVD_HOME" "/home/leo/.config/.android/avd"))
      (vterm "android-adb")
      (vterm-send-string cmd)
      (vterm-send-return)
      )
    )
  )

(defun my/tool-mapt-android-adb-proxy ()
  (interactive)
  (lambda () (interactive) (message "TODO: implement me"))
  )

;; -------------------------
;; *MAPT->iOS*

(transient-define-prefix my/tool-mapt-ios ()
  [:class transient-row "Tool -> MAPT -> iOS \n"
          ("1" "shell" my/tool-mapt-ios-shell)
          ("2" "iOS-tools" my/tool-mapt-ios-tools)
          ("3" "fs" my/tool-mapt-ios-fs)
          ("4" "proxy" my/tool-mapt-ios-set-proxy)
          ]
  )

(defun my/tool-mapt-ios-shell ()
  ;: NOTE: assume to have a proper ~/.ssh/config that works for iOS specifically
  (interactive)
  (let* ((cmd (format "ssh mobile-ios"))
         )
      (vterm "ios-shell")
      (vterm-send-string cmd)
      (vterm-send-return)
    )
  )

(transient-define-prefix my/tool-mapt-ios-fs ()
  [:class transient-row "Tool -> MAPT -> iOS -> fs\n"
          ("1" "/" my/tool-mapt-ios-fs-root)
          ("2" "screenshots" my/tool-mapt-ios-fs-screenshots)
          ]
  )


(defun my/tool-mapt-ios-fs-root ()
  ;: NOTE: assume to have a proper ~/.ssh/config that works for iOS specifically
  (interactive)
  (find-file "/scp:mobile-ios:/")
  )

(defun my/tool-mapt-ios-fs-screenshots ()
  ;: NOTE: assume to have a proper ~/.ssh/config that works for iOS specifically
  (interactive)
  (find-file "/scp:mobile-ios:/var/mobile/Media/DCIM/100APPLE/")
  )

(transient-define-prefix my/tool-mapt-ios-tools ()
  [:class transient-row "Tool -> MAPT -> iOS -> iOS Tools \n"
          ("1" "shell" my/tool-mapt-ios-tools-shell)
          ("2" "objection" my/tool-mapt-ios-tools-objection)
          ("3" "fridump" my/tool-mapt-ios-tools-fridump)
          ]
  )

(defun my/tool-mapt-ios-set-proxy ()
  (interactive)
  ;; TODO: implement this:
  ;;
  ;; - extact my own IP address
  ;; - convert from device the file '/Library/Preferences/SystemConfiguration/preferences.plist' into XML with plutil and download it
  ;; - mofify data accordingly, specifically add IP and PORT
  ;; - upload the file into remote device
  ;; - restart network services (read ios-jailbreak -> Set Proxy Programmatically)
  (message "TO BE IMPLEMENTED!")
  )

(defun my/tool-mapt-ios-tools-shell ()
  (interactive)
  (let* ()
    (vterm "ios-tools")
    (vterm-send-string "cd /home/leo/tool/ios/")
    (vterm-send-return)
    (vterm-send-string ". python-venv/bin/activate")
    (vterm-send-return)
    )
  )

(defun my/tool-mapt-ios-tools-list-apps ()
  ;; TODO: implement this
  (interactive)
  (let* ((cmd (concat ". /home/leo/tool/ios/python-venv/bin/activate && "
                      "frida-ps -Uai | grep -v '   -'"))
         (output (shell-command-to-string cmd))
         (output (cl-subseq (split-string output "\n") 2))
         )
    (delq nil (mapcar (lambda (entry)
                        (read (concat "(" entry ")"))
                        )
                      output))
    )
  )

(defun my/tool-mapt-ios-tools-objection ()
  (interactive)
  (let* ((app-list (my/tool-mapt-ios-tools-list-apps))
         (app-names (mapcar (lambda (entry) (nth 2 entry)) app-list))
         (selected-app (ivy-read "Select App: " app-names))
         (cmd (format "objection --gadget '%s' explore" selected-app))
         )
    ;; TODO: finish implementation: construct the final payload using
    ;; fridump, then create folder with fridump-<APP-NAME>-<DATETIME>
    ;; and then execute the command to obtain the dump.
    (vterm "ios-objection")
    (vterm-send-string ". /home/leo/tool/ios/python-venv/bin/activate")
    (vterm-send-return)
    (vterm-send-string cmd)
    (vterm-send-return)
    )
  )

(defun my/tool-mapt-ios-tools-fridump ()
  (interactive)
  (let* ((app-list (my/tool-mapt-ios-tools-list-apps))
         (app-names (mapcar (lambda (entry) (nth 1 entry)) app-list))
         (selected-app (ivy-read "Select App: " app-names))
         )
    ;; TODO: finish implementation: construct the final payload using
    ;; fridump, then create folder with fridump-<APP-NAME>-<DATETIME>
    ;; and then execute the command to obtain the dump.
    (message "%S" selected-app)
    )
  )

;; ------------------
;; *NPT*

(transient-define-prefix my/tool-npt ()
  [:class transient-row "Tool -> NPT \n"
          ("1" "nmap" my/tool-npt-nmap)
          ("2" "gobuster" my/tool-npt-gobuster)
          ("3" "legba" my/tool-npt-nmap)
          ]
  my/tool-system-management-ui
  )

(defun my/tool-npt-execute-cmd (buffer-name cmd)
  (let* ((display-buffer-alist '((".*" (display-buffer-no-window))))
         )
    (async-shell-command cmd buffer-name nil)
    (message (format "Started `%s`" cmd))
    )
  )

;; ------------------
;; *NPT->NMAP*

(transient-define-prefix my/tool-npt-nmap ()
  [:class transient-row "Tool -> NPT -> nmap\n"
          ("1" "basic" my/tool-npt-nmap-basic)
          ("2" "full" my/tool-npt-nmap-full)
          ("3" "udp" my/tool-npt-nmap-udp)
          ]
  my/tool-system-management-ui
  )

;; nmap -sC -sV <HOST>
(defun my/tool-npt-nmap-basic ()
  (interactive)
  (let* ((host (ivy-read "Host: " nil :initial-input ""))
         (cmd (format "nmap -sC -sV %s" host))
         (buffer-name (format "*NPT-nmap-basic-%s*" host))
         )
    (my/tool-npt-execute-cmd buffer-name cmd)
    )
  )

;; nmap -p <HOST>
(defun my/tool-npt-nmap-full ()
  (interactive)
  (let* ((host (ivy-read "Host: " nil :initial-input ""))
         (cmd (format "nmap -p- %s" host))
         (buffer-name (format "*NPT-nmap-full-%s*" host))
         )
    (my/tool-npt-execute-cmd buffer-name cmd)
    )
  )

;; nmap -sU -p161 <HOST>
(defun my/tool-npt-nmap-udp ()
  (interactive)
  ;; TODO: implement me
  )

;; ------------------
;; *NPT->GOBUSTER*

(transient-define-prefix my/tool-npt-gobuster ()
  [:class transient-row "Tool -> NPT -> nmap\n"
          ("1" "dir" my/tool-npt-gobuster-dir)
          ("2" "vhost" my/tool-npt-gobuster-vhost)
          ("3" "dns" my/tool-npt-gobuster-dns)
          ]
  my/tool-system-management-ui
  )

(defun my/tool-npt-gobuster-dir ()
  (interactive)
  ;; TODO: implement me
  )

(defun my/tool-npt-gobuster-vhost ()
  (interactive)
  ;; TODO: implement me
  )

(defun my/tool-npt-gobuster-dns ()
  (interactive)
  ;; TODO: implement me
  )

;; ------------------
;; *SCR*

(transient-define-prefix my/tool-scr ()
  [:class transient-row "Tool -> SCR -> Flow Tracking\n"
          ("1" "New Flow" my/tool-scr-new-flow)
          ("2" "Add Snippet" my/tool-scr-add-snippet)
          ]
  [:class transient-row "Tool -> SCR -> Diffing\n"
          ("df" "Files" my/tool-scr-diff-files)
          ("dd" "Directories" my/tool-scr-diff-dirs)
          ]

  [:class transient-row "Tool -> SCR -> Tooling\n"
          ("C-c 1" "Ghidra" my/tool-scr-ghidra)
          ]
  my/tool-system-management-ui
  )

(defun my/tool-scr-new-flow ()
  (interactive)
  (let* ((flow-name (ivy-read "Flow Name: " nil))
         (flow-lang (ivy-read "Flow Language: " '(java)))
         (buffer-name (format "*scr-%s*" flow-name))
         )

    (with-current-buffer (get-buffer-create buffer-name)
      (org-mode)
      (erase-buffer)
      (insert (format "#+TITLE: SCR-%s-%s\n" flow-lang flow-name))
      (insert (format "#+DATE: %s\n" (format-time-string "[%Y-%m-%d %a %H-%M-%S]")))
      (insert "\n")

      (setq-local name flow-name)
      (setq-local lang flow-lang)
      )
    )
  )

(defun my/tool-scr-select-flow ()
  (let* ((flows-buffers-list (seq-filter
                              (lambda (x) (s-starts-with? "*scr-" x))
                              (mapcar (function buffer-name) (buffer-list))))
         (selected-flow (ivy-read "Select Flow: " flows-buffers-list))
         )
    (get-buffer selected-flow)
    )
  )

(defun my/tool-scr-add-snippet (start end)
  (interactive "r")
  (let* ((buffer-flow (my/tool-scr-select-flow))
         (region-text (buffer-substring-no-properties start end))
         (filepath (buffer-file-name))
         (filename (file-name-nondirectory filepath))
         (start-line (line-number-at-pos start))
         (org-link (format "[[%s::%d][%s]]" filepath start-line filename))
         )
    (with-current-buffer buffer-flow
      ;; NOTE: to access a short filepath you can use the local
      ;; variable 'root', which contains the root directory for the
      ;; activity. Right now we don't use it, so we skip it.
      ;;
      ;; (insert (substring filepath (length root)))

      (end-of-buffer)
      (insert org-link)
      (insert "\n")
      (insert (format
               (concat
                "#+begin_src java\n"
                "%s\n"
                "#+end_src\n")
               region-text))
      (insert "\n")
      )
    )
  )

(defun my/tool-scr-diff-files ()
  (interactive)
  (let* ((file1 (nth 0 (find-file-read-args "Left file: " t)))
         (file2 (nth 0 (find-file-read-args "Right file: " t)))
         )
    (ediff file1 file2)
    )
  )

(defun my/tool-scr-diff-dirs ()
  (interactive)
  (let* ((dir1 (nth 0 (find-file-read-args "Left dir: " t)))
         (dir2 (nth 0 (find-file-read-args "Right dir: " t)))
         )
    (ztree-diff dir1 dir2)
    )
  )

(defun my/tool-src-ghidra ()
  (interactive)
  (with-environment-variables (("_JAVA_AWT_WM_NONREPARENTING" "1"))
    ;; TODO: check if ghidra exists
    (my/tool-execute-cmd "/home/leo/tool/ghidra/ghidra_11.2_PUBLIC/ghidraRun")
    )
  )

;; ------------------
;; *REPORT*

(transient-define-prefix my/tool-report ()
  [:class transient-row "Tool -> Report\n"
          ("1" "Pwndoc" pwndoc-ui)
          ("2" "New Issue" my/tool-report-new-issue)
          ;; ("3" "Search Issue" my/tool-report-search-issue)
          ]
  my/tool-system-management-ui
  )

(defun my/tool-report-new-issue ()
  (interactive)
  (let* ((ivy-sort-functions-alist nil)
         ;;
         ;; The reference file depends on the type of issue. For now
         ;; we support only WAPT, in the future the idea is to expand
         ;; to MAPT and SCR as well.
         ;;
         (type (read (ivy-read "Type: " '(wapt mapt scr))))
         (issues-ref
          (cond ((eq type 'wapt) "/home/leo/notes/denote/20241004T115723--wapt-issue__security_web.org")
                ;;
                ;; the code should never reach the catch-all
                ;; condition, if it does, we have to trigger an error
                ;;
                (t (error (format "Value %s not yet supported" type)))))
         ;;
         ;; Read issue names from official reference and select the
         ;; one we're interested in. Once selection has taken place,
         ;; we read the same file and extract only the outline of
         ;; interest to write in a new buffer that will be created for
         ;; writing the new issue.
         ;;
         (issues (org-map-entries
                  (lambda ()
                    (let* ((text (org-get-heading t t))
                           )
                      (set-text-properties 0 (length text) nil text)
                      text))
                  "LEVEL=2"
                  (list issues-ref)
                  ))
         (selected-issue (ivy-read "Issue: " issues))
         ;; To extract the content we iterate over the entire issues
         ;; file, and when we match with respect to the selected
         ;; issue, we iterate internally to extract all elements of
         ;; interest, including Summary, Description, PoC and
         ;; Summary. These can later be accessed using default-assoc.
         (selected-issue-content
          (nth 0 (delq nil (org-map-entries
                            (lambda ()
                              (when (string-equal (org-get-heading t t) selected-issue)
                                (org-map-entries
                                 (lambda ()
                                   (let* ((heading (org-get-heading))
                                          (entry (org-get-entry))
                                          )
                                     (set-text-properties 0 (length heading) nil heading)
                                     `(,heading . ,entry)
                                     )
                                   )
                                 t 'tree)))
                            "LEVEL=2"
                            (list issues-ref)
                            ))
               ))
         (buffer-filename (format "*issue-%s-%s*" type selected-issue))
         (old-buff (get-buffer-create buffer-filename))
         )
         ;; (buff
         ;;  ;;
         ;;  ;; if buffer already exists, ask if you want to clear it, or
         ;;  ;; simply append content at the end
         ;;  ;;
    ;;  (if old-buff
    ;;      (progn
    ;;          (with-current-buffer old-buff
    ;;            (if (read (ivy-read (format "Clear: %s ?" buffer-filename) '(t nil)))
    ;;                (erase-buffer)
    ;;              (end-of-buffer)
    ;;              )
    ;;            old-buff))
    ;;    (progn (get-buffer-create buffer-filename)
    ;;          )
    ;;  )
    ;; )
    ;;
    ;; open new buffer and write the issue template within the buffer
    ;;
    (with-current-buffer old-buff
      (org-mode)
      (insert (format "#+TITLE: %s\n" selected-issue))
      (insert (format "#+TYPE: %s\n" type))
      (insert "\n")
      (insert (format "* %s\n" selected-issue))
      (insert "** Summary\n" )
      (insert (assoc-default "Summary" selected-issue-content))
      (insert "\n")
      (insert "** Description\n" )
      (insert (assoc-default "Description" selected-issue-content))
      (insert "\n")
      (insert "** PoC\n" )
      (insert (assoc-default "PoC" selected-issue-content))
      (insert "\n")
      (insert "** Remediation\n" )
      (insert (assoc-default "Remediation" selected-issue-content))
      (insert "\n")
      )
    (switch-to-buffer old-buff)
    (beginning-of-buffer)
    (org-shifttab 2)
    )
  )

(defun my/tool-report-search-issue ()
  (interactive)
  )

;; ------------------
;; *VM*

(transient-define-prefix my/tool-vm ()
  ["Tool -> VM -> Status\n"
   ("-" (lambda () (interactive))
    :description (lambda () (format "VM running: %s" (mapconcat (lambda (entry) entry) (my/tool-vm-list 'running) ", "))))
   ("-" (lambda () (interactive))
    :description (lambda () (format "VM stopped: %s" (mapconcat (lambda (entry) entry) (my/tool-vm-list 'stopped) ", "))))
   ]

  [:class transient-row "Tool -> VM -> Commands\n"
   ("s" "start" my/tool-vm-start)
   ("d" "stop" my/tool-vm-stop)
   ("v" "view" my/tool-vm-view)
   ("m" "manage" (lambda () (interactive) (my/tool-execute-as-root "virt-manager")))
   ]

  my/tool-system-management-ui
  )

(defun my/tool-vm-list (state)
  (let* ((cmd (if (eq state 'running)
                  "virsh list --name --state-running --all"
                "virsh list --name --state-shutoff --all"
                ))
         (cmd-output (my/cmd-to-string-as-root cmd))
         (output (split-string cmd-output "\n"))
         )
    output
    )
  )

(defun my/tool-vm-select (running)
  (let* ((vm-list (my/tool-vm-list running))
         (selected-vm (ivy-read "Select VM: " vm-list))
         )
    selected-vm
    )
  )

(defun my/tool-vm-list-networks ()
  (split-string (my/cmd-to-string-as-root "sudo virsh net-list --name") "\n")
  )

(defun my/tool-vm-start ()
  (interactive)
  (let* ((selected-vm (my/tool-vm-select 'stopped))
         (active-networks (my/tool-vm-list-networks))
         (is-default-network-active? (cl-member "default" active-networks :test #'string-match))
         )
    (when (not is-default-network-active?)
      (my/tool-execute-as-root "virsh net-start default")
      )
    (my/tool-execute-as-root (format "virsh start %s" selected-vm))
    )
  )

(defun my/tool-vm-stop ()
  (interactive)
  (let* ((selected-vm (my/tool-vm-select 'running))
         )
    (my/tool-execute-as-root (format "virsh destroy %s" selected-vm))
    )
  )

(defun my/tool-vm-view ()
  (interactive)
  (let* ((selected-vm (my/tool-vm-select 'running))
         (spice-uri-cmd (format "virsh domdisplay --type spice %s" selected-vm))
         (spice-uri (my/cmd-to-string-as-root spice-uri-cmd))
         )
    (my/tool-execute-as-root (format "spicy --uri=%s" spice-uri))
    )
  )

;; ------------------
;; *NETWORK*

(transient-define-prefix my/tool-misc-network ()
  [:class transient-row "Tool -> Network -> Namespaces\n"
          ("-" (lambda () (interactive))
           :description (lambda () (format
                                    "Namespaces: %s"
                                    (mapconcat (lambda (entry) entry) (my/tool-misc-network-namespace-list) ", ")
                                    )
                          )
           )
          ]

  [:class transient-row ""
          ("c" "create" my/tool-misc-network-namespace-create)
          ("d" "delete" my/tool-misc-network-namespace-delete)
          ("s" "shell" my/tool-misc-network-namespace-shell)
          ]

  [:class transient-row "Tool -> Network -> VPNs \n"
          ("-" (lambda () (interactive))
           :description (lambda () (format
                                    "VPNs: %s"
                                    (mapconcat (lambda (entry) (replace-regexp-in-string " :" "-" entry))
                                               my/tool-misc-network-vpn-active ", ")
                                    )
                          )
           )
          ]


  [:class transient-row ""
          ("vc" "create" my/tool-misc-network-vpn-start)
          ("vd" "delete" my/tool-misc-network-vpn-stop)
          ("vv" "check" my/tool-misc-network-vpn-check)
          ]

  [:class transient-row "Tool -> Network -> Tools \n"
          ("1" "dns" my/tool-misc-network-dns)
          ("2" "ssh" my/grep)
          ("3" "rdp" my/grep)
          ]

  my/tool-system-management-ui
  )


(defun my/tool-misc-network-namespace-list ()
  (interactive)
  (let* ((res (split-string (my/cmd-to-string "ip netns ls | awk -F' ' '{print $1}'") "\n"))
         )
    (if (string= (nth 0 res) "")
        nil
      res)
    )
  )

(defun my/tool-misc-network-namespace-select ()
  (interactive)
  (let* ((namespace (ivy-read "Namespace: " (my/tool-misc-network-namespace-list)))
         )
    namespace
    )
  )

(defun my/tool-misc-network-namespace-create ()
  (interactive)
  (let* ((ivy-sort-functions-alist nil)
         (namespace-name (ivy-read "Namespace Name: " nil))
         (interface (ivy-read "Interface: "(split-string (my/cmd-to-string "ip -o link show | awk -F': ' '{print $2}'") "\n")))
         (vpn-profile (my/tool-misc-network-vpn-select-profile))
         (local-veth "veth0")
         (netns-veth "veth1")
         )
    (my/tool-execute-as-root
     (concat (format "ip netns add %s" namespace-name) " && "
             (format "ip netns exec %s ip link set lo up" namespace-name) " && "
             (format "mkdir -p /etc/netns/%s" namespace-name) " && "
             (format "echo \"nameserver 8.8.8.8\" | sudo tee /etc/netns/%s/resolv.conf" namespace-name) " && "
             (format "ip link add %s type veth peer name %s" local-veth netns-veth) " && "
             (format "ip link set %s netns %s" netns-veth namespace-name) " && "
             (format "ip addr add 10.200.1.1/24 dev %s" local-veth) " && "
             (format "ip link set %s up" local-veth) " && "
             (format "ip netns exec %s ip addr add 10.200.1.2/24 dev %s" namespace-name netns-veth) " && "
             (format "ip netns exec %s ip link set %s up" namespace-name netns-veth) " && "
             (format "ip netns exec %s ip route add default via 10.200.1.1" namespace-name) " && "
             (format "sudo iptables -t nat -A POSTROUTING -s 10.200.1.0/24 -o %s -j MASQUERADE" interface)
             ))

    (when env/work
      (my/tool-execute-as-root
       (concat (format "ip netns exec %s mount -t cgroup2 cgroup2 /sys/fs/cgroup" namespace-name) " && "
               (format "ip netns exec %s mount -t securityfs securityfs /sys/kernel/security/" namespace-name)
               )
       )
      )

    (my/tool-misc-network-vpn-start namespace-name vpn-profile)
    )
  )

(defun my/tool-misc-network-namespace-delete ()
  (interactive)
  (let* ((namespace (my/tool-misc-network-namespace-select))
         )
    (my/tool-execute-as-root (format "sudo ip netns del %s" namespace))
    )
  )

(defun my/tool-misc-network-namespace-shell ()
  (interactive)
  (let* ((netns-name (my/tool-misc-network-namespace-select))
         )
    (vterm (format "netns-%s" netns-name))
    (vterm-send-string (format "sudo ip netns exec %s bash" netns-name))
    (vterm-send-return)
    )
  )

(defun my/tool-misc-network-namespace-cmd-format (namespace)
  (cond
   ((string-equal namespace "host")
    (format "bash -c \"%%s\"")
    )
   (t
    (format "sudo ip netns exec %s bash -c \"%%s\"" namespace)
    )
   )
  )

(defvar my/tool-misc-network-vpn-profiles
  `(
    (:none . ((:name . :none)
              (:start . "true")
              (:stop . "true")
              (:check . "true")))
    (:mullvad . ((:name . :mullvad)
                 (:start . "sudo wg-quick up /home/leo/tool/wireguard/mullvad.conf")
                 (:stop . "sudo wg-quick down /home/leo/tool/wireguard/mullvad.conf >/dev/null 2>&1")
                 (:check . "")
                 ))

    (:work . ((:name . :work)
              (:start . "F=$(mktemp) && echo $P > $F && openvpn --askpass $F --config /home/leo/projects/MINDED/archive/vpn/openvpn.ovpn && sleep 1 && rm $F")
              (:stop . "sudo pkill openvpn")
              (:check . "")
              ))
    )
  )

;; (with-environment-variables (("P" "Co2aA_wras%"))
;;   (let* (
;;       (vpn-cmd "F=$(mktemp) && echo $P > $F && openvpn --askpass $F --config /home/leo/projects/MINDED/archive/vpn/openvpn.ovpn && sleep 1 && rm $F")
;;       )
;;     (my/tool-execute-as-root vpn-cmd)
;;     )
;;   )

(defvar my/tool-misc-network-vpn-active nil)

(defun my/tool-misc-network-vpn-select-profile ()
  (assoc-default (read (ivy-read "VPN Profile: "
                                 (mapcar (lambda (entry) (car entry)) my/tool-misc-network-vpn-profiles)
                                 ))
                 my/tool-misc-network-vpn-profiles)
  )

;; TODO: implement me
(defun my/tool-misc-network-vpn-start (&optional netn vpn)
  (interactive)
  (let* ((namespace (if netn
                        netn
                      (ivy-read "Namespace: " (append '("host") (my/tool-misc-network-namespace-list)))))
         (namespace-cmd (my/tool-misc-network-namespace-cmd-format namespace))
         (is-host? (string-equal namespace "host"))

         (vpn-profile (if vpn
                          vpn
                        (my/tool-misc-network-vpn-select-profile)))
         (vpn-name (assoc-default :name vpn-profile))
         (vpn-cmd (assoc-default :start vpn-profile))

         ;; this only happens for minded VPN
         (vpn-password (when (equal :work vpn-name) (read-passwd "VPN Password: ")))

         ;; TODO: figure out how to start work VPN from within a
         ;; custom network namespace. The problem here is that to
         ;; launch this command I need to access some env variables,
         ;; and this is not possible with the bash -c construct.
         (final-cmd (format namespace-cmd vpn-cmd))
         )
    (with-environment-variables (("P" vpn-password))
      (my/tool-execute-as-root final-cmd)
      (add-to-list 'my/tool-misc-network-vpn-active (format "%s %s" namespace vpn-name))
      )
    )
  )

(defun my/tool-misc-network-vpn-stop ()
  (interactive)
  (let* ((vpn-conn  (ivy-read "VPN to Stop: " my/tool-misc-network-vpn-active))

         (namespace-name (nth 0 (split-string vpn-conn " ")))
         (is-host? (string-equal namespace-name "host"))
         (namespace-cmd (my/tool-misc-network-namespace-cmd-format namespace-name))

         (vpn-name (nth 1 (split-string vpn-conn " ")))
         (vpn-profile (assoc-default (read vpn-name) my/tool-misc-network-vpn-profiles))
         (vpn-stop-cmd (assoc-default :stop vpn-profile))

         (final-cmd (format namespace-cmd vpn-stop-cmd))
         )
    (condition-case nil
        (my/tool-execute-as-root final-cmd)
      (error))
    (setq my/tool-misc-network-vpn-active
          (remove vpn-conn my/tool-misc-network-vpn-active))
    )
  )

;; TODO: implement me
;; curl -s https://am.i.mullvad.net/connected
(defun my/tool-misc-network-vpn-check ()
  (interactive)
  )

(defun my/tool-misc-network-dns ()
  (interactive)
  (find-file "/sudo:root@imq:/etc/hosts")
  )

;; ------------------
;; *AGENDA*

(transient-define-prefix my/tool-agenda ()
  [:class transient-row "Tool -> Agenda \n"
          ("1" "View" my/tool-agenda-open)
          ("2" "New" my/tool-agenda-new)
          ("3" "Modify" my/tool-agenda-modify)
          ]
  my/tool-system-management-ui
  )

(defun my/tool-agenda-open ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-file-source "Life" agenda-home "White")
    (cfw:org-create-file-source "Work" agenda-work "Orange")
    )
   )
  )

(defun my/tool-agenda-new ()
  "Depending on the environment we're working on, the agenda is
updated slightly different. Specifically, different org-templates
 are used."
  (interactive)
  (let* ((calendar (ivy-read "Select calendar:" '("life" "work")))
         (template (assoc-default calendar '(("life" . "cl") ("work" . "cw"))))
         )
    (org-capture nil template)
    )
  )

(defun my/tool-agenda-modify ()
  (interactive)
  ;; TODO: implement me
  ;; (org-capture nil "a")
  )

;; ------------------
;; *BACKUP*

(transient-define-prefix my/tool-backup ()
  [:class transient-row "Tool -> Backup -> Mount\n"
          ("1" "Mount" my/tool-backup-mount)
          ("2" "Unmount" my/tool-backup-unmount)
          ]
  [:class transient-row "Tool -> Backup -> Snapshots \n"
          ("3" "Init" my/tool-backup-init)
          ("4" "List" my/tool-backup-list-snapshots)
          ("5" "New" my/tool-backup-new-snapshot)
          ]
  my/tool-system-management-ui
  )

(defun my/tool-backup-list-disk-devices ()
  (let* ((cmd "fdisk -l | grep Disk | grep /dev | awk -F '[,]' '{print $1}'")
         (output (split-string (my/cmd-to-string-as-root cmd) "\n"))
         (output (mapcar (lambda (entry) (substring (nth 1 (split-string entry " ")) 0 -1)) output))
         (output (mapcar (lambda (entry) (cdr (split-string (my/cmd-to-string-as-root (format "fdisk -l | grep %s" entry)) "\n"))) output))
         (output (-flatten output))
         (output (mapcar
                  (lambda (entry)
                    (let* ((line (split-string entry "[ \t\n\r]+" t)))
                      (format "%s %s %s" (nth 0 line) (nth 4 line) (mapconcat 'identity (seq-subseq line 5 nil) " "))
                      )
                    )
                  output))
         )
    output
    )
  )

(defun my/tool-backup-mount ()
  (interactive)
  (let* ((device-raw (ivy-read "Device to mount: " (my/tool-backup-list-disk-devices)))
         (device-to-mount (nth 0 (split-string device-raw " ")))
         (mount-point (nth 0 (let* ((default-directory "/mnt"))
                        (find-file-read-args "Mount point: " t)
                        )))
         (mount-cmd (format "mount %s %s" device-to-mount mount-point))
         )
    (setq my/tool-backup-mount-point mount-point)
    (my/tool-execute-as-root mount-cmd)
    )
  )

(defun my/tool-backup-unmount ()
  (interactive)
  (when my/tool-backup-mount-point
    (my/tool-execute-as-root (format "umount %s" my/tool-backup-mount-point))
    )
  )

(defun my/tool-backup-init ()
  (interactive)
  )

(defun my/tool-backup-list-snapshots ()
  (interactive)
  (when my/tool-backup-mount-point
    (let* ((repo-path (my/ask-for-path "Backup: " my/tool-backup-mount-point))
           (cmd (format "restic -r %s snapshots" repo-path))
           )
      (my/tool-execute-as-root cmd)
      )
    )
  )

(defun my/tool-backup-new-snapshot ()
  (interactive)
  (when my/tool-backup-mount-point
    (let* ((backup (my/ask-for-path "Backup: " my/tool-backup-mount-point))
           (dir-to-backup (my/ask-for-path "To Backup: " "~"))
           (cmd (format "restic -r %s --verbose backup %s" backup dir-to-backup))
           )
      (my/tool-execute-as-root cmd)
      )
    )
  )

;; ------------------
;; *LAYOUT*

(transient-define-prefix my/tool-layout ()
  [:class transient-row "Tool -> Layout\n"
          ("0" "Empty" my/tool-layout-empty)	  
          ("1" "Compilation" my/tool-layout-compilation)
          ("2" "Portswigger" my/tool-layout-portswigger)
          ("3" "Work" my/tool-layout-work)
          ]
  my/tool-system-management-ui
  )

(defun my/tool-layout-clear ()
  (interactive)
  (let* ((buff (current-buffer))
	 )
    ;; delete all tabs
    (while (> (length (tab-bar-tabs)) 1)
      (tab-bar-close-tab))
    (condition-case nil (tab-bar-close-tab) (error nil) )    

    ;; delete all windows in current tab
    (delete-other-windows)

    (switch-to-buffer buff)
    )
  )

(defun my/tool-layout-empty ()
  (interactive)
  (my/tool-layout-clear)
  (switch-to-buffer "*scratch*")
  )

(defun my/tool-layout-compilation ()
  "Setup basic window layouts for basic compilation. The frame is
  divided into three tabs:

   - In the first tab, we have three window, one for code, one
     for compilation buffer and one for vterm

   - In the second tab, we have one window, for vterm.

   - In the third tab, we have two windows, one for code, one for browser."
  (interactive)

  (my/tool-layout-clear)
  
  ;; first tab: code, compilation and vterm
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer "*compilation*")
  (split-window-vertically)
  (other-window 1)
  (switch-to-buffer "*vterminal<1>*")
  (other-window 1)

  ;; second tab: code, browser
  (tab-new)
  (split-window-horizontally)
  (other-window 1)
  (start-process "" nil (locate-file "chromium" exec-path exec-suffixes 1))
  (sleep-for 1)
  (other-window 1)

  ;; third tab
  (tab-new)
  (switch-to-buffer "*vterminal<1>*")

  ;; go back to the first tab
  (tab-next)
  )

(defun my/tool-layout-portswigger ()
  "Setup basic window layouts for basic compilation. The frame is
  divided into three tabs:

   - In the first tab, we have three window, one for code, one
     for compilation buffer and one for vterm

   - In the second tab, we have one window, for vterm.

   - In the third tab, we have two windows, one for code, one for browser."
  (interactive)

  (my/tool-layout-clear)
  
  ;; first tab: org buffer, browser with labs
  (denote-open-or-create "/home/leo/notes/denote/20241224T201346--portswigger-labs__security_web.org")
  (split-window-horizontally)
  (other-window 1)
  (browse-url "https://portswigger.net/web-security/all-labs")
  (sleep-for 1)

  ;; second tab: burpsuite, browser with proxy
  (tab-new)
  (switch-to-buffer "burp<2>")
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer "proxy")

  ;; go back to the first tab
  (tab-next)
  )

(defun my/tool-layout-work ()
  "Setup basic window layouts for work activity. The frame is
  divided into three tabs:

   - In the first tab, we have a single window for the activity.

   - In the second tab, we have one two windows, one for burp,
     one for the proxy browser.

   - In the third tab, we have the chromium core instance with
     slack."
  (interactive)

  (my/tool-layout-clear)

  ;; first tab: activity directory 
  (my/tool-work-activity-dir)

  ;; second tab: burpsuite, browser with proxy
  (tab-new)
  (switch-to-buffer "burp<2>")
  (split-window-horizontally)
  (other-window 1)
  (switch-to-buffer "proxy")

  ;; third tab: chromium with core
  (tab-new)
  (switch-to-buffer "core")  
  
  ;; go back to the first tab
  (tab-next)
  )

(defun my/tool-encoder ()
  (interactive)
  (if (featurep 'wapt)
      (let* ((from (ivy-read "From: " wapt--encoder-formats))
	     (to (ivy-read "To: " wapt--encoder-formats))
	     (input (ivy-read "Input: " nil :initial-input ""))
	     )
	(kill-new (wapt--encoder-compute from to input))
	)
    (message "WAPT.el not loaded, cannot encode!")
    )
  )
