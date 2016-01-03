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
                                 (args (ignore-errors (aref item 2))))
                             (vector title
                                     (lambda ()
                                       (interactive)
                                       (apply 'start-menu/start-process title title program (split-string-and-unquote args))))))
                          ((listp item)
                           (start-menu/translate-conf-to-menu item)))) items))))

(defvar start-menu/menu-conf '("Start"
                               ["Gvim" "D:/Program Files/Vim/vim73/gvim.exe"]
                               ("网络"
                                ["Firefox" "C:/Program Files/Mozilla Firefox/firefox.exe"]
                                ["IE" "iexplore.exe"]))
  "")

;; (start-menu/make-menu-item "C:/Program Files/Mozilla Firefox/firefox.exe")
;; => ["firefox" "C:/Program Files/Mozilla Firefox/firefox.exe" nil]
(defun start-menu/make-menu-item (program &optional name args)
  "return a new menu-item. If NAME is nil the name of new menu-item will be the basename of PROGRAM"
  (let ((name (or name (file-name-base program))))
    (vector name program args)))

;; (start-menu/make-menu "Start" (start-menu/make-menu-item "/bin/emacs") (start-menu/make-menu-item "/bin/gvim"))
;; => ("Start" ["emacs" "/bin/emacs" nil] ["gvim" "/bin/gvim" nil])
(defun start-menu/make-menu (menu-name &rest menu-items)
  "return a new menu which name is MENU-NAME and content is MENU-ITEMS which is a list of menu or menu-item"
  (cons menu-name menu-items))

(defun start-menu/menu-name (menu)
  "return name of MENU "
  (car menu))

(defun start-menu/menu-content (menu)
  "return content of MENU which is list of menu or menu-item"
  (cdr menu))

(defun start-menu/insert-into-menu-first (menu &rest submenu-or-items)
  "Insert SUBMENU-OR-ITEMS at the beginning of MENU, return the new menu"
  (let ((menu-name (start-menu/menu-name menu))
        (menu-new-items (append submenu-or-items (start-menu/menu-content menu))))
    (cons menu-name menu-new-items)))

(defun start-menu/insert-into-menu-last (menu &rest submenu-or-items)
  "Insert SUBMENU-OR-ITEMS at the ending of MENU, return the new menu"
  (append menu submenu-or-items))

(defun start-menu/menu-p (object)
  (listp object))

(defun start-menu/menu-item-p (object)
  (vectorp object))

(defun start-menu/find-submenu (menu submenu-name)
  "find submenu which name is SUBMENU-NAME of MENU "
  (let ((submenus (cl-remove-if-not #'start-menu/menu-p (start-menu/menu-content menu))))
    (cl-find-if (lambda (menu)
                  (string-equal submenu-name (start-menu/menu-name menu)))
                submenus)))

(defun start-menu/exist-in-p (submenu-or-items menu)
  "Is SUBMENU-OR-ITEMS exist in MENU"
  (cond ((start-menu/menu-p submenu-or-items)
         (let ((submenu-name (start-menu/menu-name submenu-or-items)))
           (start-menu/find-submenu menu (submenu-name))))
        ((start-menu/menu-item-p submenu-or-items)
         (let* ((menu-items-already-exist (cl-remove-if-not #'start-menu/menu-item-p (start-menu/menu-content menu))))
           (member submenu-or-items menu-items-already-exist)))
        (t (error "Invalid Data Type: %s" submenu-or-items))))

(defun start-menu/add-to-menu-content-first (menu submenu-or-item)
  "If SUBMENU-OR-ITEM did not exist in MENU, insert it into MENU. otherwise do nothing"
  (unless (start-menu/exist-in-p submenu-or-item menu)
    (start-menu/insert-into-menu-first menu submenu-or-item)))

(defun start-menu/add-to-menu-content-last (menu submenu-or-item)
  "If SUBMENU-OR-ITEM did not exist in MENU, insert it into MENU. otherwise do nothing"
  (unless (start-menu/exist-in-p submenu-or-item menu)
    (start-menu/insert-into-menu-last menu submenu-or-item)))

(easy-menu-define start-menu global-map
  "menu for start"
  (start-menu/translate-conf-to-menu start-menu/menu-conf))

(provide 'start-menu)
