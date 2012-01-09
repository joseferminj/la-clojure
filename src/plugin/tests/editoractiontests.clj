(ns plugin.tests.editoractiontests
  (:use [clojure.test :only [deftest is testing]])
  (:require plugin.test))

(deftest barf-backwards-tests
  (is (editor-action-result? "org.jetbrains.plugins.clojure.actions.editor.BarfBackwardsAction"
                             "(a (b<caret> c) d e)"
                             "(a b (c) d e)")))

(deftest barf-forwards-tests
  (is (editor-action-result? "org.jetbrains.plugins.clojure.actions.editor.BarfForwardsAction"
                             "(a (b<caret> c) d e)"
                             "(a (b) c d e)")))

(deftest slurp-backwards-tests
  (is (editor-action-result? "paredit-backward-slurp-sexp"
                               "(foo bar (baz<caret> quux) zot)"
                               "(foo (bar baz<caret> quux) zot)"))
    (is (editor-action-result? "paredit-backward-slurp-sexp"
                               "(a b ((c<caret> d)) e f)"
                               "(a (b (c<caret> d)) e f)"))
    (is (editor-action-result? "paredit-backward-slurp-sexp"
                               "foo bar (<caret>)"
                               "foo (bar)"))
    (is (editor-action-result? "paredit-backward-slurp-sexp"
                               "(foo bar(<caret>))"
                               "(foo (bar))")))

(deftest slurp-forwards-tests
  (is (editor-action-result? "paredit-forward-slurp-sexp"
                             "(foo (bar <caret>baz) quux zot)"
                             "(foo (bar <caret>baz quux) zot)"))
  (is (editor-action-result? "paredit-forward-slurp-sexp"
                             "(a b ((c<caret> d)) e f)"
                             "(a b ((c<caret> d) e) f)"))
  (is (editor-action-result? "paredit-forward-slurp-sexp"
                             "(<caret>) foo bar"
                             "(<caret>foo) bar"))
  (is (editor-action-result? "paredit-forward-slurp-sexp"
                             "((<caret>) foo bar)"
                             "((<caret>foo) bar)")))

(deftest splice-tests
  (is (editor-action-result? "org.jetbrains.plugins.clojure.actions.editor.SpliceAction"
                             "(a (b c <caret>d) e)"
                             "(a b c d e)")))

(deftest paredit-kill
  (testing "within s-exps"
           (is (editor-action-result? "paredit-kill-action"
                                      "(a<caret> b c)"
                                      "(a)"))
           (is (editor-action-result? "paredit-kill-action"
                                      "(<caret> a  b c)"
                                      "()"))
           (is (editor-action-result? "paredit-kill-action"
                                      "(<caret> a  b (c d))"
                                      "()")))
  (testing "in the clojure file"
           (is (editor-action-result? "paredit-kill-action"
                                      (str "<caret>(a b \n"
                                           "a)"
                                           "(1 2 3)")
                                      "(1 2 3)"))
           (is (editor-action-result? "paredit-kill-action"
                                      (str
                                        "<caret>(a b c)\n"
                                        "(1 2 3)")
                                      "(1 2 3)"))
           (is (editor-action-result? "paredit-kill-action"
                                      "<caret>    (a b c)\n"
                                      "\n"))
           (is (editor-action-result? "paredit-kill-action"
                                      (str
                                        "(a b c)\n"
                                        "noel<caret>ement\n"
                                        "(1 2 3)")
                                      (str
                                        "(a b c)\n"
                                        "(1 2 3)"))))
  (is (editor-action-result? "paredit-kill-action"
                             (str
                               "<caret>\n"
                               "(a b c)")
                             "(a b c)")))


(deftest kill-word-forward
  (testing "with s-exps and comments"
           (is (editor-action-result? "paredit-forward-kill-word"
                                      "<caret>(foo bar)  ; baz"
                                      "(<caret>bar)  ; baz"))
           (is (editor-action-result? "paredit-forward-kill-word"
                                      "(<caret> bar)  ; baz"
                                      "(<caret>)  ; baz"))
           (is (editor-action-result? "paredit-forward-kill-word"
                                      "(<caret>)  ; baz"
                                      "()  ;<caret>"))
           (is (editor-action-result? "paredit-forward-kill-word"
                                      "(foo )  ;<caret>"
                                      "(foo )  ;<caret>")))

  (testing "more with comments"
           (is (editor-action-result? "paredit-forward-kill-word"
                                      "(foo bar)  ; baz<caret> jose hola"
                                      "(foo bar)  ; baz hola"))
           (is (editor-action-result? "paredit-forward-kill-word"
                                      (str
                                        "(a b c)  ; baz<caret>\n"
                                        "(foo bar)")
                                      (str
                                        "(a b c)  ; baz\n"
                                        "(<caret>bar)"))))
  (testing "with defn and comments"
           (is (editor-action-result? "paredit-forward-kill-word"
                                      (str
                                        ";;;<caret>Frobnicate\n"
                                        "(defn frobnicate ...)")
                                      (str
                                        ";;;<caret>\n"
                                        "(defn frobnicate ...)")))
           (is (editor-action-result? "paredit-forward-kill-word"
                                      (str
                                        ";;;<caret>\n"
                                        "(defn frobnicate ...)")
                                      (str ";;;\n"
                                           "(<caret>frobnicate ...)"))))
  (testing "string support"
           (is (editor-action-result? "paredit-forward-kill-word"
                                      "(foo <caret>\"bar baz\" quux)"
                                      "(foo \"<caret> baz\" quux)"))
           (is (editor-action-result? "paredit-forward-kill-word"
                                      "(foo \"<caret> baz\" quux)"
                                      "(foo \" \" quux)"))
           (is (editor-action-result? "paredit-forward-kill-word"
                                      "(foo \"<caret>\" quux)"
                                      "(foo \"\"<caret>)"))))
(deftest kill-work-backward
  (testing "with s-exps and comments"
           (is (editor-action-result? "paredit-backward-kill-word"
                                      (str "(foo bar)    ; baz\n"
                                           "(quux)<caret>")
                                      (str "(foo bar)    ; baz\n"
                                           "(<caret>)")))
           (is (editor-action-result? "paredit-backward-kill-word"
                                      (str "(foo bar)    ; baz\n"
                                           "(<caret>)")
                                      (str "(foo bar)    ; <caret>\n"
                                           "()")))
           (is (editor-action-result? "paredit-backward-kill-word"
                                      (str "(foo bar)    ;<caret>\n"
                                           "()")
                                      (str "(foo<caret>)    ;\n"
                                           "()")))
           (is (editor-action-result? "paredit-backward-kill-word"
                                      (str "(foo <caret>)    ;\n"
                                           "()")
                                      (str "(<caret>)    ;\n"
                                           "()")))))