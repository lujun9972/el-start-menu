;; -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun start-menu/start-process (name buffer program &rest program-args)
  "start a program, the buffer will be killed after program exit"
  (set-process-sentinel (apply 'start-process name buffer program program-args)
						(lambda (proc event)
						  (let ((buf (process-buffer proc)))
							(delete-process proc)
							(unless (get-buffer-process buf)
							  (kill-buffer buf))))))

(defun start-menu/translate-conf-to-menu (menu)
  (let ((menu-name (car menu))
        (items (cdr menu)))
    (cons menu-name
          (mapcar (lambda (item)
                    (cond ((vectorp item)
                           (let ((title (aref item 0))
                                 (program (aref item 1))
                                 (args (or (ignore-errors (aref item 2))
                                           "")))
                             (vector title
                                     (lambda ()
                                       (interactive)
                                       (apply 'start-menu/start-process title title program (split-string-and-unquote args))))))
                          ((listp item)
                           (start-menu/translate-conf-to-menu item)))) items))))

(defvar start-menu/menu-conf '("Start"
                               ["Gvim" "gvim"]
                               ("网络"
                                ["Firefox" "firefox"]
                                ["Chrome" "chromium-browser"]))
  "")

;; (setq item-firefox (start-menu/make-menu-item "C:/Program Files/Mozilla Firefox/firefox.exe"))
;; => ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil]
(defun start-menu/make-menu-item (program &optional name args)
  "return a new menu-item. If NAME is nil the name of new menu-item will be the basename of PROGRAM"
  (let ((name (or name (file-name-base program))))
    (vector name program args)))

;; (setq menu-start (start-menu/make-menu "Start" (start-menu/make-menu-item "/bin/emacs") (start-menu/make-menu-item "/bin/gvim")))
;; => ("Start" ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu/make-menu (menu-name &rest menu-items)
  "return a new menu which name is MENU-NAME and content is MENU-ITEMS which is a list of menu or menu-item"
  (cons menu-name menu-items))

;; (start-menu/menu-name menu-start)
;; => "Start"
(defun start-menu/menu-name (menu)
  "return name of MENU "
  (car menu))

;; (start-menu/menu-content menu-start)
;; =>(["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu/menu-content (menu)
  "return content of MENU which is list of menu or menu-item"
  (cdr menu))

;; (start-menu/insert-into-menu-first menu-start (start-menu/make-menu "Internet"))
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
;; menu-start
;; => ("Start" ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu/insert-into-menu-first (menu &rest submenu-or-items)
  "Insert SUBMENU-OR-ITEMS at the beginning of MENU, return the new menu"
  (let ((menu-name (start-menu/menu-name menu))
        (menu-new-items (append submenu-or-items (start-menu/menu-content menu))))
    (apply #'start-menu/make-menu menu-name menu-new-items)))

;; (start-menu/insert-into-menu-first! menu-start (start-menu/make-menu "Internet"))
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
;; menu-start
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu/insert-into-menu-first! (menu &rest submenu-or-items)
  "Insert SUBMENU-OR-ITEMS at the beginning of MENU, return the new menu. MENU will be alerted"
  (let ((menu-name (start-menu/menu-name menu))
        (menu-new-items (append submenu-or-items (start-menu/menu-content menu))))
    (setf (cdr menu) menu-new-items)
    menu))

;; (setq item-chrome (start-menu/make-menu-item "chrome"))
;; (start-menu/insert-into-menu-last menu-start item-chrome)
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil])
;; menu-start
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu/insert-into-menu-last (menu &rest submenu-or-items)
  "Insert SUBMENU-OR-ITEMS at the ending of MENU, return the new menu"
  (append menu submenu-or-items))

;; (start-menu/insert-into-menu-last! menu-start item-chrome)
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil])
;; menu-start
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil])
(defun start-menu/insert-into-menu-last! (menu &rest submenu-or-items)
  "Insert SUBMENU-OR-ITEMS at the ending of MENU, return the new menu. MENU will be alerted"
  (nconc menu submenu-or-items))

;; (start-menu/menu-p menu-start)
;; => t
;; (start-menu/menu-p item-firefox)
;; => nil
(defun start-menu/menu-p (object)
  (listp object))

;; (start-menu/menu-item-p menu-start)
;; => nil
;; (start-menu/menu-item-p item-firefox)`
;; => t
(defun start-menu/menu-item-p (object)
  (vectorp object))

;; (setq internet-menu (start-menu/find-submenu menu-start '("Internet")))
;; => ("Internet")
;; (start-menu/find-submenu menu-start '("Internet" "Browser"))
;; => nil
;; (setq Browser-menu (start-menu/find-submenu menu-start '("internet" "Browser") t))
;; => ("Browser")
;; menu-start
;; => ("Start" ("Internet") ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil] ("internet" ("Browser")))
(defun start-menu/find-submenu (menu submenu-path-list &optional create-p)
  "find submenu which located by SUBMENU-PATH-LIST in MENU.

If CREATE is non-nil, it will create submenu by SUBMENU-PATH-LIST"
  (if (or (null menu)
          (null submenu-path-list))
      menu
    (let* ((submenu-name (car submenu-path-list))
           (submenus (cl-remove-if-not #'start-menu/menu-p (start-menu/menu-content menu)))
           (submenu (cl-find-if (lambda (menu)
                                  (string-equal submenu-name (start-menu/menu-name menu)))
                                submenus)))
      (when (and create-p
                 (null submenu))
        (setq submenu (start-menu/make-menu submenu-name))
        (start-menu/insert-into-menu-last! menu submenu))
      (start-menu/find-submenu submenu (cdr submenu-path-list) create-p))))

;; (start-menu/exist-in-p internet-menu menu-start)
;; => ("Internet")
;; (start-menu/exist-in-p item-firefox menu-start)
;; => nil'
;; (start-menu/exist-in-p item-chrome menu-start)
;; => (["chrome" "chrome" nil])
(defun start-menu/exist-in-p (submenu-or-items menu)
  "Is SUBMENU-OR-ITEMS exist in MENU"
  (cond ((start-menu/menu-p submenu-or-items)
         (let ((submenu-name (start-menu/menu-name submenu-or-items)))
           (start-menu/find-submenu menu (list submenu-name))))
        ((start-menu/menu-item-p submenu-or-items)
         (let* ((menu-items-already-exist (cl-remove-if-not #'start-menu/menu-item-p (start-menu/menu-content menu))))
           (member submenu-or-items menu-items-already-exist)))
        (t (error "Invalid Data Type: %s" submenu-or-items))))

;; (start-menu/add-to-menu-content-first internet-menu item-firefox)
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil])
;; internet-menu
;; => ("Internet")
(defun start-menu/add-to-menu-content-first (menu submenu-or-item)
  "If SUBMENU-OR-ITEM did not exist in MENU, insert it into MENU. otherwise do nothing"
  (unless (start-menu/exist-in-p submenu-or-item menu)
    (start-menu/insert-into-menu-first menu submenu-or-item)))

;; (start-menu/add-to-menu-content-first! internet-menu item-firefox)
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil])
;; (start-menu/add-to-menu-content-first! internet-menu item-firefox)
;; => nil
;; internet-menu
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil])
;; menu-start
;; => ("Start" ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil]) ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil] ("internet" ("Browser")))
(defun start-menu/add-to-menu-content-first! (menu submenu-or-item)
  "If SUBMENU-OR-ITEM did not exist in MENU, insert it into MENU(MENU will be alerted). otherwise do nothing"
  (unless (start-menu/exist-in-p submenu-or-item menu)
    (start-menu/insert-into-menu-first! menu submenu-or-item)))

;; (start-menu/add-to-menu-content-last internet-menu item-chrome)
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil] ["chrome" "chrome" nil])
;; internet-menu
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil])
;; menu-start
;; => ("Start" ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil]) ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil] ("internet" ("Browser")))
(defun start-menu/add-to-menu-content-last (menu submenu-or-item)
  "If SUBMENU-OR-ITEM did not exist in MENU, insert it into MENU. otherwise do nothing"
  (unless (start-menu/exist-in-p submenu-or-item menu)
    (start-menu/insert-into-menu-last menu submenu-or-item)))

;; (start-menu/add-to-menu-content-last! internet-menu item-chrome)
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil] ["chrome" "chrome" nil])
;; (start-menu/add-to-menu-content-last! internet-menu item-chrome)
;; => nil
;; internet-menu
;; => ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil] ["chrome" "chrome" nil])
;; menu-start
;; => ("Start" ("Internet" ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil] ["chrome" "chrome" nil]) ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil] ["chrome" "chrome" nil] ("internet" ("Browser")))
(defun start-menu/add-to-menu-content-last! (menu submenu-or-item)
  "If SUBMENU-OR-ITEM did not exist in MENU, insert it into MENU(MENU will be alerted). otherwise do nothing. "
  (unless (start-menu/exist-in-p submenu-or-item menu)
    (start-menu/insert-into-menu-last! menu submenu-or-item)))

(easy-menu-define start-menu global-map
  "menu for start"
  (start-menu/translate-conf-to-menu start-menu/menu-conf))

(provide 'start-menu)
