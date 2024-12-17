(defpackage lw-dash
  (:add-use-defaults))

(in-package lw-dash)

(defconstant +docset-name+ "CAPI.docset")
(defconstant +docset-documents-path+ "Contents/Resources/Documents/")

(defconstant +docset-plist-info-filename+ "Contents/Info.plist")

(defparameter *docset-plist-info-format-string*
             "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<!DOCTYPE plist PUBLIC \"-//Apple//DTD PLIST 1.0//EN\" \"http://www.apple.com/DTDs/PropertyList-1.0.dtd\">
<plist version=\"1.0\">
<dict>
  <key>CFBundleIdentifier</key>
  <string>~A</string>
  <key>CFBundleName</key>
  <string>LispWorks ~A</string>
  <key>DocSetPlatformFamily</key>
  <string>~A</string>
  <key>isDashDocset</key>
  <true/>
  <key>DashDocSetFamily</key>
  <string>dashtoc</string>
  <key>dashIndexFilePath</key>
  <string>~A.htm</string>
  <key>isJavaScriptEnabled</key><false/>
  ~A
</dict>
</plist>")

(defconstant +docset-db-filename+ "Contents/Resources/docSet.dsidx")

;; Default doc paths

(defparameter *docs-base-dir*
  (merge-pathnames (format nil "lib/8-0-0-0/manual/html-~A/"
                           #+mswindows "w"
                           #+darwin "m"
                           #-(or mswindows darwin) "u")
                   *lispworks-directory*))

(defun doc-folder (name)
  (merge-pathnames (string-append (string-downcase name) "/") *docs-base-dir*))

;; Plist

(defun create-plist-info (name dest &optional fulltext-search)
  "Create the proper Info.plist file in the destination DEST"
  (let* ((folder-name (string-append (string-upcase name) ".docset"))
         (docset-plist-fname (string-append (namestring (truename dest)) folder-name "/" +docset-plist-info-filename+)))
    (with-open-file (f docset-plist-fname
                       :if-exists :supersede
                       :direction :output)
      (format f *docset-plist-info-format-string*
              (string-downcase name) (string-upcase name) (string-downcase name) (string-downcase name)
              (if fulltext-search "<key>DashDocSetDefaultFTSEnabled</key><true/>" "")))))

;; Icon

(defconstant +docset-icon-filename+ "icon.png")
(defconstant +docset-icon-filename2x+ "icon@2x.png")

(defun create-icon (name dest)
  "Create some (interesting) icons for your docset :P"
  (let ((dest-image (string-append (namestring (truename dest)) (string-upcase name) ".docset" "/" +docset-icon-filename+))
        (dest-image2x (string-append (namestring (truename dest)) (string-upcase name) ".docset" "/" +docset-icon-filename2x+))
        (port (capi:contain (make-instance 'capi:output-pane) :display-state :hidden)))
    ;; Draw something similar... with an initial on it :P
    (gp:with-pixmap-graphics-port (pixmap port 16 16 :background :transparent :clear t)
      (let* ((font (gp:find-best-font pixmap (gp:make-font-description
                                              :family "Courier New" :size 16 :weight :bold)))
             (char (char (string-upcase name) 0))
             (width (gp:get-char-width pixmap char font))
             (ascent (gp:get-char-ascent pixmap char font)))
        (gp:draw-circle pixmap (* 16 0.35) 8 (* 8 0.9) :foreground :deepskyblue2 :filled t)
        (gp:draw-circle pixmap (* 16 0.65) 8 8 :foreground :dodgerblue2 :filled t)
        (gp:draw-circle pixmap 8 8 6 :foreground :white :filled t)
        (gp:draw-character pixmap char (/ (- 16 width) 2) ascent :font font :foreground :midnightblue)
        (gp:externalize-and-write-image pixmap (gp:make-image-from-port pixmap) dest-image)))
    (gp:with-pixmap-graphics-port (pixmap port 32 32 :background :transparent :clear t)
      (let* ((font (gp:find-best-font pixmap (gp:make-font-description
                                              :family "Courier New" :size 32 :weight :bold)))
             (char (char (string-upcase name) 0))
             (width (gp:get-char-width pixmap char font))
             (ascent (gp:get-char-ascent pixmap char font)))
        (gp:draw-circle pixmap (* 32 0.35) 16 (* 16 0.9) :foreground :deepskyblue2 :filled t)
        (gp:draw-circle pixmap (* 32 0.65) 16 16 :foreground :dodgerblue2 :filled t)
        (gp:draw-circle pixmap 16 16 12 :foreground :white :filled t)
        (gp:draw-character pixmap char (/ (- 32 width) 2) ascent :font font :foreground :midnightblue)
        (gp:externalize-and-write-image pixmap (gp:make-image-from-port pixmap) dest-image2x)))))

;; Entry parsing

(defstruct entry name type href)

(defun parse-type-str (str default)
  (setq str (string-trim-whitespace str))
  (macrolet ((match (&body body)
               (let ((r 'default))
                 (dolist (forms body)
                   (setq r `(if (member str ',(car forms) :test #'equal) (progn ,@(cdr forms)) ,r)))
                 r)))
    (match
     (("Function" "Functions" "Accessor" "Accessors" "C Function" "Backward Chaining Goal" "Conflict Resolution Tactic / Function") "Function")
     (("Macro" "Macros" "Symbol Macro" "Local Macro") "Macro")
     (("Generic Function" "Generic Functions" "Accessor Generic Function") "Method")
     (("Class" "Classes" "System Class" "System Classes" "Condition Class" "Condition Classes" "Java Classes" "Abstract Class") "Class")
     (("Variable" "Variables" "Editor Variable" "Editor Variables") "Variable")
     (("Constant" "Constants") "Constant")
     (("Type" "Types" "FLI Type Descriptor" "FLI Type Descriptors") "Type")
     (("Special Form" "External Format" "External Formats") "Builtin")
     (("Parameter") "Parameter")
     (("Keyword" "Trace keyword") "Keyword")
     (("Editor Command" "Editor Commands" "Debugger command" "Listener command") "Command"))))

(defun parse-type (dom &optional (default "Guide"))
  "Parse the type of a page based on its 'REntry-inline-type'"
  (let ((nodes (clss:select ".REntry-inline-type" dom)))
    (if (> (length nodes) 0)
      (parse-type-str (plump-dom:text (aref (plump-dom:children (aref nodes 0)) 0))
                      default)
      default)))

;; Evaluate following script to get all possible REntry-inline-types
;; and In-line-interface-types
#|
(let (r)
  (dolist (type '(:capi :corba :deliv :editor :fli :ide :kw :lw :objc))
    (dolist (file (directory (merge-pathnames "*.htm" (doc-folder type))))
      (let* ((dom (plump-parser:parse (alexandria:read-file-into-string file)))
             (type (aref (clss:select ".REntry-inline-type" dom) 0))
             (inline-type (clss:select ".In-line-interface-type" dom)))
        (when type (pushnew (plump-dom:text (aref (plump-dom:children type) 0)) r :test #'equal))
        (loop for inline across inline-type
              do (pushnew (plump-dom:text (aref (plump-dom:children inline) 0)) r :test #'equal)))))
  r)

(" Local Macro" " Constants" " Parameter" " System Classes" " External Format" " Condition Classes" " Listener command" " Types" " Debugger command" " External Formats" " Special Form" " Trace keyword" " Java Class" " Variables" " C Function" " Backward Chaining Goal" " Symbol Macro" " Conflict Resolution Tactic / Function" " Condition Class" " FLI Type Descriptors" " FLI Type Descriptor" " Editor Variables" " Editor Commands" " Editor Variable" " " " Editor Command" " Keyword" " Generic Functions" " Abstract Class" " Accessors" " Constant" " System Class" " Functions" " Macros" " Accessor Generic Function" " Accessor" " Type" " Variable" " Generic Function" " Macro" " Function" " Class")
|#

(defun select-classes (node &rest classes)
  (declare (inline select-classes))
  (apply #'concatenate 'list (mapcar (lambda (class) (clss:select class node)) classes)))

(defun parse-entries-and-insert-anchors (file)
  "Parse entries out of a LW HTML manual file, and insert Dash anchors
into its DOM.

Return two values: The entries collected, and the PLUMP:ROOT DOM that
has been modified."
  (let* ((dom (plump-parser:parse (alexandria:read-file-into-string file)))
         (type (parse-type dom))
         entries)
    (if (equal type "Guide")
      ;; In the guides of LW docs, each sub-title will be classed with
      ;; "FM'n'Heading". Based on this, we generate entries for each
      ;; subtitle
      (let ((nodes (apply #'select-classes dom
                          (loop for i from 1 to 9 collect (format nil ".FM~AHeading" i))))
            (inlines (clss:select ".In-line-interface" dom)))
        (if (> (length nodes) 0)
          (dolist (node nodes)
            (let* ((entry-name (string-trim-whitespace
                                (apply #'concatenate 'string
                                       (map 'list (lambda (child)
                                                    (if (plump-dom:text-node-p child)
                                                      (plump-dom:text child)
                                                      ""))
                                            (plump-dom:children node)))))
                   (anchor-name (format nil "//apple_ref/cpp/Guide/~A" (quri:url-encode entry-name))))
              (push (make-entry :name entry-name
                                :type "Guide"
                                :href (format nil "~A#~A" (file-namestring file) anchor-name))
                    entries)
              (let ((anchor-node (make-instance 'plump-dom:element :parent (plump-dom:parent node) :tag-name "a")))
                (plump-dom:set-attribute anchor-node "NAME" anchor-name)
                (lquery-funcs:add-class anchor-node "dashAnchor")
                (plump-dom:insert-before node anchor-node))))
          (push (make-entry :name (plump-dom:text (first (plump-dom:get-elements-by-tag-name dom "title")))
                            :type "Guide"
                            :href (file-namestring file))
                entries))
        ;; Some entries (e.g. Editor commands) are embedded inside
        ;; guides, with the "In-line-interface" class. Parse it...
        (when (> (length inlines) 0)
          (loop for inline across inlines
                for type = (parse-type-str
                            (plump-dom:text (aref (plump-dom:children (aref (clss:select ".In-line-interface-type" inline) 0)) 0))
                            "Guide")
                do (dolist (name (loop for child across (plump-dom:children inline)
                                       when (and (plump-dom:text-node-p child)
                                                 (> (length (string-trim-whitespace (plump-dom:text child))) 0))
                                         collect (string-trim-whitespace (plump-dom:text child))))
                     (let ((anchor-name (format nil "//apple_ref/cpp/~A/~A" type (quri:url-encode name))))
                       (push (make-entry :name name
                                         :type type
                                         :href (format nil "~A#~A" (file-namestring file) anchor-name))
                             entries)
                       (let ((anchor-node (make-instance 'plump-dom:element :parent (plump-dom:parent inline) :tag-name "a")))
                         (plump-dom:set-attribute anchor-node "NAME" anchor-name)
                         (lquery-funcs:add-class anchor-node "dashAnchor")
                         (plump-dom:insert-before inline anchor-node)))))))
      
      (let ((nodes (select-classes dom ".REntry" ".REntry2")))
        (dolist (node nodes)
          (dolist (link (plump-dom:get-elements-by-tag-name node :a))
            ;; For each entry, there's a list of links target to it, named in DOCSET)symbol
            ;; We parse this link to generate entries, to cover all hyperlinks in docset.
            (let* ((name (plump-dom:attribute link "NAME"))
                   (entry-name (second (split-sequence '(#\)) name))))
              (when entry-name
                (push (make-entry :name entry-name
                                  :type type
                                  :href (format nil "~A#~A" (file-namestring file) name))
                      entries)
                (let ((anchor-node (make-instance 'plump-dom:element :parent (plump-dom:parent link) :tag-name "a")))
                  (plump-dom:set-attribute anchor-node "NAME"
                                           (format nil "//apple_ref/cpp/~A/~A" type (quri:url-encode entry-name)))
                  (lquery-funcs:add-class anchor-node "dashAnchor")
                  (plump-dom:insert-before link anchor-node))))))))
    (values entries dom)))

(defun generate-docset (name dest &key source (log-stream *standard-output*) progress-bar fulltext-search)
  "Valid names are LispWorks html manual directory names, e.g. capi, deliv..."
  (unless source (setq source (doc-folder name)))
  (format log-stream "Source path: ~a~%" source)
  (format log-stream "Destination path: ~a~%" dest)
  (let* ((folder-name (string-append (string-upcase name) ".docset"))
         (docset-path (string-append (namestring (truename dest)) folder-name))
         (docs-path (string-append docset-path "/" +docset-documents-path+))
         (docset-db-fname (string-append (namestring (truename dest)) folder-name "/" +docset-db-filename+))
         db
         (fnames (directory (merge-pathnames "*.*" (namestring (truename source))))))
    
    (format log-stream "Creating ~a~%" docs-path)
    (ensure-directories-exist docs-path)
    
    (format log-stream "Creating plist.info~%")
    (create-plist-info name dest fulltext-search)
    
    (format log-stream "Initialize database~%")
    (setq db (sqlite:connect docset-db-fname))
    ;; drop previous table
    (sqlite:execute-non-query db "DROP TABLE IF EXISTS searchIndex")
    ;; create index table
    (sqlite:execute-non-query db "CREATE TABLE searchIndex(id INTEGER PRIMARY KEY, name TEXT, type TEXT, path TEXT)")
    (sqlite:execute-non-query db "CREATE UNIQUE INDEX anchor ON searchIndex (name, type, path)")
    (format log-stream "Create an icon files~%")
    (create-icon name dest)
    
    (unwind-protect
        (loop for i from 1
              for fname in fnames
              do (format log-stream "Processing file ~A/~A~%" i (length fnames))
                 (when progress-bar
                   (capi:apply-in-pane-process
                    progress-bar
                    #'(setf capi:range-slug-start)
                    (round (* (/ i (length fnames)) 100)) progress-bar))
                 (let* ((short-name (car (last (split-sequence "/" (namestring fname)))))
                        (dest-name (string-append (namestring (truename docs-path)) short-name)))
                   (unless (file-directory-p fname) ;; skip directories
                     (if (member (pathname-type fname) '("htm" "html") :test #'equal)
                       (multiple-value-bind (entries dom)
                           (parse-entries-and-insert-anchors fname)
                         (lquery-funcs:write-to-file dom dest-name)
                         (dolist (entry entries)
                           (sqlite:execute-non-query
                            db
                            "INSERT OR IGNORE INTO searchIndex(name, type, path) VALUES (?, ?, ?)"
                            (entry-name entry) (entry-type entry) (entry-href entry))))
                       (copy-file fname dest-name)))))
      (sqlite:disconnect db))
    
    (format log-stream "~%The docset ~a has been created~%DONE~%" docset-path)
    (when progress-bar
      (capi:apply-in-pane-process
       progress-bar
       (lambda ()
         (setf (capi:range-slug-start progress-bar) 0)
         (capi:popup-confirmer
          nil "Docset Generated."
          :cancel-button nil
          :buttons '("Open in Dash" "Open file location")
          :callbacks (list (lambda () (sys:call-system (string-append "open " (namestring docset-path))))
                           (lambda () (sys:call-system (string-append "open " (namestring dest)))))
          :callback-type :none
          :owner progress-bar))))))

(capi:define-interface dash-main-window ()
  ()
  (:panes
   (name-option
    capi:option-pane
    :title "Docset Name:" :title-position :left
    :items '(:capi :corba :deliv :editor :ide :kw :lw :objc :rnig))
   (output-input
    capi:text-input-pane 
    :title "Output location:"
    :text (namestring (sys:get-folder-path :documents))
    :buttons '(:browse-file (:directory t :image :std-file-open) :ok nil))
   (alternative-source-input
    capi:text-input-pane
    :title "Alternative source:" :title-position :left
    :buttons '(:browse-file (:directory t :image :std-file-open) :ok nil))
   (fulltext-search-check
    capi:check-button
    :text "Enabe Full-text search by default")
   (progress
    capi:progress-bar
    :start 0 :end 100)
   (generate
    capi:push-button
    :text "Generate"
    :callback (lambda (data itf)
                (declare (ignore data))
                (block nil
                  (with-slots (name-option output-input alternative-source-input progress) itf
                    (let ((alt-src (capi:text-input-pane-text alternative-source-input))
                          (dest (capi:text-input-pane-text output-input)))
                      (when (mp:find-process-from-name "Dash Generate Docset")
                        (return (capi:prompt-with-message "Docset is generating...")))
                      (if (> (length alt-src) 0)
                        (unless (file-directory-p alt-src)
                          (return (capi:prompt-with-message "Source is not a directory!")))
                        (setq alt-src nil))
                      (unless (file-directory-p dest)
                        (return (capi:prompt-with-message "Output location is not a directory!")))
                      (mp:process-run-function
                       "Dash Generate Docset" ()
                       #'generate-docset
                       (capi:choice-selected-item name-option) dest
                       :source alt-src :progress-bar progress
                       :fulltext-search (capi:button-selected fulltext-search-check))))))))
  (:default-initargs
   :title "Dash Docset Generator"
   :best-width '(character 60)))

(defun main ()
  (capi:display (make-instance 'dash-main-window)))

;(generate-docset :capi "~/common-lisp/dash/")
;(capi:display (make-instance 'dash-main-window))