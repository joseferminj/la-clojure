(ns plugin.actions.editor
  (:import [com.intellij.openapi.actionSystem AnAction ActionManager
            PlatformDataKeys KeyboardShortcut]
   [com.intellij.openapi.actionSystem.ex ActionManagerEx]
   [com.intellij.openapi.editor Document Editor ScrollType]
   [com.intellij.openapi.fileEditor FileDocumentManager]
   [com.intellij.openapi.project Project]
   [com.intellij.openapi.vfs VirtualFile]
   [com.intellij.openapi.keymap KeymapManager]
   [com.intellij.openapi.editor.actionSystem EditorWriteActionHandler]
   [com.intellij.psi PsiManager PsiWhiteSpace PsiElement PsiComment]
   [com.intellij.psi.impl.source.tree PsiCommentImpl]
   [com.intellij.openapi.editor.actions MoveCaretLeftOrRightHandler]
   [org.jetbrains.plugins.clojure.actions.editor PareditKillAction ClojureEditorAction]
   [org.jetbrains.plugins.clojure.psi ClojurePsiElement]
   [org.jetbrains.plugins.clojure.psi.util ClojurePsiUtil]
   [org.jetbrains.plugins.clojure.psi.api ClBraced ClLiteral]
   [org.jetbrains.plugins.clojure.psi.api.symbols ClSymbol]
   [org.jetbrains.plugins.clojure.lexer ClojureTokenTypes]))

; Action registration
(defn register-action [action id]
  (let [am (ActionManager/getInstance)
        group (.getAction am "ClojureStructuralEditingActionGroup")]
    (do
      (.registerAction am id action)
      (.add group action))))


(defn create-handler [f]
  (proxy [EditorWriteActionHandler] []
         (executeWriteAction [editor context]
                             (f editor context))))

(defn create-editor-action [f]
  (proxy [ClojureEditorAction] [(create-handler f)]))

(defn set-text [action text]
  (do
    (.. action getTemplatePresentation (setText text))
    action))

(defn- get-key-stroke [k]
       (ActionManagerEx/getKeyStroke k))

(defn register-shortcut
      ([action-id first-key]
       (register-shortcut action-id first-key nil))
  ([action-id first-key second-key]
   (let [km (.getKeymap (KeymapManager/getInstance) "$default")
         first (get-key-stroke first-key)
         second (if-not (nil? second-key) (get-key-stroke second-key))]
     (.addShortcut km action-id (KeyboardShortcut. first second)))))

(defn defeditor-action [id text shortcut handler]
  (do
    ; Create action
    (-> (create-editor-action handler) (set-text text) (register-action id))
    ; Register shortcut
    (if (vector? shortcut)
        (let [[f s] shortcut]
          (register-shortcut id f s))
        (register-shortcut id shortcut))))


; Editor manipulation
(defn coerce-editor [editor]
  "Given a editor from IDEA, it creates a map for easier manipulation"
  (when-let [project (.getProject editor)]
            (let [document (.getDocument editor)]
              (when-let [vfile (-> (FileDocumentManager/getInstance) (.getFile document))]
                        (when-let [file (-> (PsiManager/getInstance project) (.findFile vfile))]
                                  {:editor editor :file file})))))

(defn get-offset [{:keys [editor] :as e}]
  (merge e {:offset (.. editor getCaretModel getOffset)}))

(defn element-at [{:keys [offset file]}]
  (.findElementAt file offset))

(defn last-child-in-file [{:keys [file]}]
  (.getLastChild file))

(defn sexp? [^PsiElement e]
  (instance? ClBraced e))

(defn sexp-at [^PsiElement e]
  (if (or
        (nil? e)
        (sexp? e))
      e
      (recur (.getParent e))))

(defn at-sexp? [^PsiElement e]
  (if-let [parent (.getParent e)]
          (sexp? parent)
          false))


(defn cut-line [editor context]
  (doto (com.intellij.openapi.editor.actions.CutLineEndAction.)
        (.actionPerformed editor context)))

(defn delete! [el]
  (.delete el))

(defn next-sibling [el]
  (.getNextSibling el))

(defn prev-sibling [el]
  (.getPrevSibling el))


(defn next-siblings [el]
  ;  (lazy-seq
  (if (nil? el)
      []
      (conj (next-siblings (next-sibling el)) el)))
;)

(defn wsp? [el]
  (instance? PsiWhiteSpace el))

(defn first-brace? [e]
  (and
    (at-sexp? e)
    (= e (.. e getParent getFirstBrace))))

(defn last-brace? [e]
  (and
    (at-sexp? e)
    (= e (.. e getParent getLastBrace))))

(defn start-offset [el]
  (.. el getTextRange getStartOffset))

(defn end-offset [el]
  (.. el getTextRange getEndOffset))

(defn at-current-line? [{:keys [editor offset]} el]
  (let [doc (.getDocument editor)
        line (.getLineNumber doc offset)
        el-offset (start-offset el)
        el-line (.getLineNumber doc el-offset)]
    (= line el-line)))

;" Kill a line as if with CutLineEndAction, but respecting delimiters.
;
; On a line with no S-expressions on it starting after the point or
; within a comment, act exactly as CutLineEndAction. Otherwise, kill all
; S-expressions that start after the point. In a string, act exactly
; as CutLineEndAction but do not kill past the closing string delimiter.
;
; See paredit-kill http://mumble.net/~campbell/emacs/paredit.html
;"
(defeditor-action "paredit-kill-action"
                  "Paredit Kill"
                  "alt k"
                  (fn [editor context]
                      (let [e (-> editor coerce-editor get-offset)
                            el (element-at e)
                            parent (if-not (nil? el) (.getParent el))]
                        (cond
                          ; Never call
                          (nil? el) ()
                          ; When the caret is in the first brace, delete the sexp
                          (first-brace? el) (delete! parent)
                          ; When the caret is in the sexp, delete all next siblings except whitespaces until last brace
                          (at-sexp? el) (doseq [s (->> (next-siblings el)
                                                       (filter #(and (not (wsp? %))
                                                                     (not (= (.getLastBrace parent) %)))))]
                                               (delete! s))
                          ; When the caret is not in a sexp, the next element
                          :else (let [next-el (->> el next-siblings (drop-while #(wsp? %)) first)]
                                  ; if the next element is at the current line, delete it
                                  (if (at-current-line? e next-el)
                                      (delete! next-el)
                                      ;otherwise act like cut-line action
                                      (cut-line editor context)))))))

(defn move-caret-at [editor offset]
  (do
    (-> editor (.getCaretModel) (.moveToOffset offset))
    (-> editor (.getScrollingModel) (.scrollToCaret (ScrollType/RELATIVE)))))

(defn move-caret-to-element [editor el]
  (move-caret-at editor (start-offset el)))


(defn comment? [e]
  (instance? PsiComment e))

(defn split-comment [s]
  [(apply str (take-while #(= \; %) s))
   (apply str (drop-while #(= \; %) s))])

(defn delete-trailing-wsps [string]
  (drop-while #(= \space %) string))

(defn delete-first-word [string]
  (drop-while #(not= \space %) string))

(defn delete-word-in-string [offset text]
  (let [prefix (subs text 0 offset)]
    (->> text
         (drop offset)
         delete-trailing-wsps
         delete-first-word
         (concat prefix)
         (apply str))))


(defn delete-word-in-comment [editor offset comment]
  (let [start (start-offset comment)
        text (.getText comment)
        [comment-token comment-text] (split-comment text)
        roffset (- offset start)]
    (if (< roffset (count comment-token))
        (do
          (move-caret-at editor (+ start (count comment-token)))
          (.updateText comment (delete-word-in-string (count comment-token) text)))
        (.updateText comment (delete-word-in-string roffset text)))))

(defn literal? [e]
  (instance? ClLiteral e))

(defn string-literal? [e]
  (= (.. e getNode getElementType) ClojureTokenTypes/STRING_LITERAL))


(defn string-literal-end? [offset e]
  (let [start (start-offset e)
        l (.. e getTextRange getLength)]
    (= (- offset start) (dec l))))

(defn delete-word-in-literal [editor offset string]
  (let [start (start-offset string)
        text (.getText string)
        text-without (subs text 1 (dec (count text)))
        roffset (inc (- offset start 1))]
    (do
      (move-caret-at editor (inc start))
      (.replaceWithText string
                        (str "\""
                             (delete-word-in-string roffset text-without)
                             "\"")))
    nil))




(defeditor-action "paredit-forward-kill-word"
                  "Foward Kill Word"
                  "alt d"
                  (fn [editor context]
                      (let [e (-> editor coerce-editor get-offset)
                            offset (:offset e)]
                        (loop [el (element-at e)]
                          (cond
                            (nil? el) ()
                            (comment? el) (delete-word-in-comment editor offset el)
                            (and
                              (string-literal? el)
                              (not (string-literal-end? offset el))) (delete-word-in-literal editor offset el)
                            (or
                              (sexp? el)
                              (literal? el)) (recur (.getFirstChild el))
                            (or
                              (wsp? el)
                              (first-brace? el)) (recur (next-sibling el))
                            (or
                              (last-brace? el)
                              (string-literal? el)) (recur (next-sibling (.getParent el)))
                            :else (do
                                    (move-caret-to-element editor el)
                                    (delete! el)))))))

(defn at-comment-token? [offset comment]
  (let [end-offset (end-offset comment)
        o (if (> offset end-offset) end-offset offset)
        roffset (- o (start-offset comment))
        t (subs (.getText comment) 0 roffset)]
    (println "Roffset:" roffset "Offset: " offset "Start:" (start-offset comment) "Text: " t "Endoffset: " end-offset)
    (not-any? #(and (not= \space %) (not= \; %)) t)))

(defn delete-word-backward-in-comment [editor offset comment]
  (let [end (end-offset comment)
        text (.getText comment)
        [comment-token comment-text] (split-comment text)
        roffset (- end offset)]
    (cond
      (< roffset 0) (do
                      (move-caret-at editor end)
                      (.updateText comment (->> (delete-word-in-string 0 (->> text reverse (apply str)))
                                                reverse
                                                (apply str))))

      () (.updateText comment (delete-word-in-string roffset text)))))

(defeditor-action "paredit-backward-kill-word"
                  "Backward Kill Word"
                  "alt b"
                  (fn [editor context]
                      (let [e (-> editor coerce-editor get-offset)
                            offset (:offset e)]
                        (loop [el (element-at e)]
                          (println "Element-at: " el "OffSet:" offset)
                          (cond
                            (nil? el) (recur (last-child-in-file e))
                            (and
                              (comment? el)
                              (at-comment-token? offset el)) (recur (prev-sibling el))
                            (comment? el) (delete-word-backward-in-comment editor offset el)
                            (sexp? el) (recur (.getLastChild el))
                            (or
                              (wsp? el)
                              (last-brace? el)) (recur (prev-sibling el))
                            (first-brace? el) (recur (prev-sibling (.getParent el)))
                            :else (do
                                    (move-caret-to-element editor el)
                                    (delete! el)))))))

(defn clojure-element? [el]
  (instance? ClojurePsiElement el))

(defn- next-element [el f-next]
       (if (not (nil? el))
           (let [s (f-next el)]
             (if (or (nil? s)
                     (clojure-element? s))
                 s
                 (recur s f-next)))))

(defn- find-parent-with-sibling [el f-next brace?]
       (if (not (nil? el))
           (if-let [parent (.getParent el)]
                   (if (and
                         (sexp? parent)
                         (not (nil? (f-next parent)))
                         (not (-> parent f-next brace?)))
                       parent
                       (recur parent f-next brace?)))))

(defn make-slurp-action [f-next brace? handler]
  (fn [editor context]
      (let [e (-> editor coerce-editor get-offset)
            offset (:offset e)
            el (element-at e)
            sexp (find-parent-with-sibling el f-next brace?)
            slurpee (next-element sexp f-next)]
        (handler editor sexp slurpee))))

(defeditor-action "paredit-forward-slurp-sexp"
                  "Slurp Forward"
                  "ctrl shift 0"
                  (make-slurp-action next-sibling
                                     last-brace?
                                     (fn [_ sexp slurpee]
                                         (do
                                           (let [copy (.copy slurpee)]
                                             (delete! slurpee)
                                             (.addAfter sexp copy (-> sexp .getLastChild prev-sibling)))))))

(defn move-caret-to-element-end [editor el]
  (move-caret-at editor (start-offset el)))

(defeditor-action "paredit-backward-slurp-sexp"
                  "Slurp Backward"
                  "ctrl shift 9"
                  (make-slurp-action prev-sibling
                                     first-brace?
                                     (fn [editor sexp slurpee]
                                         (do
                                           (let [copy (.copy slurpee)]
                                             (delete! slurpee)
                                             (.addBefore sexp copy (-> sexp .getFirstChild next-sibling)))))))
