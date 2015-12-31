;; -*- lexical-binding: t; -*-

(defun start-menu/start-process (name buffer program &rest program-args)
  "start a program, the buffer will be killed after program exit"
  (set-process-sentinel (apply 'start-process name buffer program program-args)
						(lambda (proc event)
						  (let ((buf (process-buffer proc)))
							(delete-process proc)
							(unless (get-buffer-process buf)
							  (kill-buffer buf))))))

(defun start-menu/translate-conf-to-menu (menu)
  (let ((name (car menu))
        (items (cdr menu)))
    (cons name (mapcar (lambda (item)
                         (cond ((vectorp item)
                                (let ((title (aref item 0))
                                      (path (aref item 1)))
                                  (vector title
										  (lambda ()
											(interactive)
											(start-menu/start-process title title path)))))
                               ((listp item)
                                (start-menu/translate-conf-to-menu item)))) items))))

(defvar start-menu/menu-conf '("Start"
                               ["Gvim" "D:/Program Files/Vim/vim73/gvim.exe"]
                               ("网络"
                                ["Firefox" "C:/Program Files/Mozilla Firefox/firefox.exe"]
                                ["IE" "iexplore.exe"]))
  "")
(setq start-menu/menu-conf '("Start"
							 ["Gvim" "D:/Program Files/Vim/vim73/gvim.exe"]
							 ("网络"
							  ["Firefox" "C:/Program Files/Mozilla Firefox/firefox.exe"]
							  ["IE" "iexplore.exe"]))
	  )

(easy-menu-define start-menu global-map
  "menu for start"
  (start-menu/translate-conf-to-menu start-menu/menu-conf))
