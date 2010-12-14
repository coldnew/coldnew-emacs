;; vimpulse-test.el --- unit tests for Vimpulse -*- coding: utf-8 -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Created: May 2010
;; Keywords: unit testing

;; This file is NOT part of Vimpulse proper.

;;; Commentary:

;; This file is for developers only. It runs a couple of unit tests on
;; Vimpulse (BOTTOM OF FILE). To load it, add this line to .emacs:
;;
;;     (require 'vimpulse-test)
;;
;; Yeah, I am rolling my own framework. The pre-existing frameworks
;; all had their good points, but none offered every feature I needed.
;; Plus it's more fun this way. :)

;;; Tests

;; A simple test may look like this:
;;
;;     (deftest test-foo
;;       (assert (= (+ 2 2) 4))
;;       (assert (= (* 3 3) 9))
;;       (assert (= (% 4 2) 0)))
;;
;; This checks that 2 + 2 = 4, that 3 * 3 = 9, and that 4 % 2 = 0.
;; (If it isn't, something is seriously wrong!) To run the test:
;;
;;     (test-foo)    ; `M-x test-foo' interactively
;;
;; Alternatively, specify `:run t' when defining the test to run the test
;; every time the `deftest' form is evaluated:
;;
;;     (deftest test-foo
;;       :run t
;;       (assert (= (+ 2 2) 4))
;;       (assert (= (* 3 3) 9))
;;       (assert (= (% 4 2) 0)))
;;
;; Let's simplify it a bit. The various `assert-' forms accept multiple (sets
;; of) arguments similarly to e.g. `setq':
;;
;;     (deftest test-foo
;;       :run t
;;       (assert
;;         (= (+ 2 2) 4)
;;         (= (* 3 3) 9)
;;         (= (% 4 2) 0)))
;;
;; This is Lisp, after all! Furthermore, `assert-=' removes the
;; `=' noise (and makes the failure report more specific):
;;
;;     (deftest test-foo
;;       :run t
;;       (assert-=
;;         (+ 2 2) 4
;;         (* 3 3) 9
;;         (% 4 2) 0))
;;
;; At this point it's advisable to add some documentation. Tests as
;; well as assertions can have docstrings:
;;
;;     (deftest test-foo
;;       "Example test."
;;       :run t
;;       (assert-=
;;         "Elementary truths."
;;         (+ 2 2) 4
;;         (* 3 3) 9
;;         (% 4 2) 0))
;;
;; If the test fails, the first line of the docstring shows up in the report.
;;
;; NOTE: xUnit frameworks often use "Yoda order", e.g.,
;; "assertEquals(4, 2 + 2);", where the expected value comes first.
;; Here, however, the expected value follows the actual. (Although
;; `=' is symmetrical, the failure reports depend on the order.)
;;
;; NOTE 2: `assert' only accepts multiple arguments inside `deftest'.
;; Outside `deftest' it's a different macro (defined by cl.el).
;;
;; NOTE 3: it's a good idea to name tests with a prefix, e.g.,
;; "test-". This avoids accidently overwriting other functions.

;;; Test suites

;; Tests can be grouped into suites with `defsuite'. The most
;; straightforward way is to just wrap it around them:
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :run t
;;       (deftest test-foo
;;         "Example test."
;;         (assert-=
;;           "Elementary truth."
;;           (+ 2 2) 4))
;;       (deftest test-bar
;;         "Another example test."
;;         (assert-=
;;           "More elementary truth."
;;           (* 3 3) 9)))
;;
;; Like tests, the suite is executed with (test-foo-suite),
;; `M-x test-foo-suite' or `:run t' in the definition. Suites can also
;; have docstrings. For brevity, "deftest" can be omitted:
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :run t
;;       (test-foo
;;        "Example test."
;;        (assert-=
;;          "Elementary truth."
;;          (+ 2 2) 4))
;;       (test-bar
;;        "Another example test."
;;        (assert-=
;;          "More elementary truth."
;;          (* 3 3) 9)))
;;
;; One can go even further and remove the test names themselves:
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :run t
;;       ("Example test."
;;        (assert-=
;;          "Elementary truth."
;;          (+ 2 2) 4))
;;       ("Another example test."
;;        (assert-=
;;          "More elementary truth."
;;          (* 3 3) 9)))
;;
;; This might be suitable for small suites, but in general it's better
;; to give each test a name by which they can be called afterwards.
;;
;; A suite can include other suites simply by listing the suite names
;; in its `defsuite' form. Furthermore, `defsuite' forms may be
;; nested. One can also define the test suite first and then add tests
;; and suites to it, using the `:suite' keyword or `add-to-suite':
;;
;;     (defsuite test-foo-suite
;;       "Example suite.")
;;
;;     (deftest test-foo
;;       "Example test."
;;       :suite test-foo-suite
;;       (assert-=
;;         "Elementary truth."
;;         (+ 2 2) 4))
;;
;;     (add-to-suite 'test-foo-suite 'test-bar)
;;
;; NOTE: self-referencing suite definitions should be avoided,
;; although some safeguards exist to prevent infinite loops.

;;; Fixtures

;; Sometimes it's useful to set up and tear down an environment for
;; each test in a suite. This can be done with the :setup and
;; :teardown keyword arguments, which accept a list of expressions to
;; evaluate before and after each test.
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :setup ((wibble) (wobble))
;;       :teardown ((wubble) (flob))
;;       (test-foo
;;        ...)
;;       (test-bar
;;        ...))
;;
;; However, this may not be enough: what if the setup and teardown
;; need to share variables, or the test should be wrapped in a macro
;; like `save-restriction'? To that end, the more powerful :fixture
;; keyword argument may be used. It accepts a one-argument function
;; which is used to call the test:
;;
;;     (defsuite test-foo-suite
;;       "Example suite."
;;       :fixture (lambda (body)
;;                  (let (foo bar)
;;                    (wibble)
;;                    (wobble)
;;                    (save-restriction
;;                      (funcall body))   ; run test
;;                    (wubble)
;;                    (flob)))
;;       (test-foo
;;        ...)
;;       (test-bar
;;        ...))
;;
;; There's also the :wrap keyword argument, which specifies an
;; around-advice for the whole test, e.g., :wrap ((wobble) ad-do-it).
;; (See `defadvice' for more details.) The arguments are executed in
;; the following order:
;;
;;                  +-------------------+
;;                  |:wrap              |
;;                  |  +--------------+ |
;;                  |  |:setup        | |
;;                  |  |  +---------+ | |
;;                  |  |  |:fixture | | |
;;                  |  |  |  +----+ | | |
;;                  |  |  |  |TEST| | | |
;;                  |  |  |  +----+ | | |
;;                  |  |  +---------+ | |
;;                  |  |:teardown     | |
;;                  |  +--------------+ |
;;                  +-------------------+
;;
;; Any single test may also specify these arguments. In that case, the
;; suite fixtures are wrapped around the test fixtures.
;;
;; NOTE: if defining a function to use as a fixture, make sure it's
;; defined before the tests are run (before the test if using :run t).
;;
;; NOTE 2: :setup, :teardown and :fixture are repeated for each test
;; in the suite, while :wrap is executed once for the whole suite.
;;
;; NOTE 3: a test defined as part of a suite carries with it the
;; suite's fixtures even when called outside the suite. When the test
;; is called by a different suite, that suite's fixtures temporarily
;; override the fixtures inherited from the original suite.

;;; Mocks and stubs

;; Mocks and stubs are temporary stand-ins for other pieces of code.
;; They are useful for disabling (or "stubbing out") external behavior
;; while testing a unit.
;;
;; To stub a function, use `stub':
;;
;;     (deftest test-foo
;;       "Example test."
;;       (stub foo)
;;       (assert-not (foo)))  ; foo returns nil
;;
;; In the rest of the test, any calls to the stubbed function will
;; return nil. To return a different value, specify the stub's body,
;; e.g., (stub foo t).
;;
;; A stub only changes a function's output, not its input: the
;; argument list remains the same. The stub's body may refer to the
;; original arguments. To change a function's input too, use `mock':
;;
;;     (deftest test-foo
;;       "Example test."
;;       (mock foo (arg)
;;         (1+ arg))
;;       (assert-= (foo 1) 2))  ; foo returns 2
;;
;; `mock' specifies a temporary function to assign to `foo' for the
;; duration of the test. Here it increments its argument by one. When
;; the test completes (or fails), all stubs and mocks are released.
;;
;; If the same mock is frequently reused, put it in a fixture or
;; define a function for it and call that function in the test. Just
;; ensure that it is never called outside a test, otherwise it will
;; not be released (unless wrapped in `with-mocks-and-stubs').

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'advice)
  (require 'warnings))

(eval-and-compile
  (defvar all-suites nil)
  (defvar all-tests nil)
  (defvar current-suite nil)
  (defvar test-passed nil)
  (defvar suite-passed nil)
  (defvar silent-tests nil
    "If t, don't echo test results.")
  (defvar logged-tests t
    "If t, log echoed test results in the *Messages* buffer.")
  (defvar deftest-macros nil
    "Macros that shadow global definitions inside `deftest'."))

;;; Test suite macro: `defsuite'

(defmacro defsuite (suite &rest body)
  "Define a test suite."
  (declare (indent defun)
           (debug (&define name
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((parents (when (and (boundp 'current-suite)
                             current-suite)
                    (list current-suite)))
         (current-suite suite)
         doc form keyword lambda test-forms run
         debug fixture setup teardown wrap tests)
    ;; Collect docstring.
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; Collect keywords.
    (while (keywordp (car body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :run)
        (setq run (pop body)))
       ((memq keyword '(:parent :suite))
        (if (listp (car body))
            (setq parents (append parents (pop body)))
          (add-to-list 'parents (pop body))))
       ((eq keyword :fixture)
        (setq fixture (pop body)))
       ((eq keyword :setup)
        (setq setup (pop body)))
       ((eq keyword :teardown)
        (setq teardown (pop body)))
       ((memq keyword '(:advice :wrap))
        (setq wrap (pop body)))
       ((eq keyword :debug)
        (setq debug (pop body)))
       (t
        (pop body))))
    ;; Collect "abbreviated" forms -- that is, test definitions
    ;; lacking the `deftest' symbol and/or a test name.
    (while (setq form (pop body))
      (cond
       ((symbolp form)
        (add-to-list 'tests form))
       ((not (cdr form))
        (add-to-list 'tests (car form)))
       ((and (symbolp (car form))
             (fboundp (car form))
             (not (test-p (car form))))
        (add-to-list 'test-forms form))
       (t
        (add-to-list 'test-forms (append '(deftest) form)))))
    (unless (or (symbolp fixture)
                (functionp fixture))
      (setq fixture `(lambda () ,@fixture)))
    (unless (or (symbolp setup)
                (functionp setup))
      (setq setup `(lambda () ,@setup)))
    (unless (or (symbolp teardown)
                (functionp teardown))
      (setq teardown `(lambda () ,@teardown)))
    ;; Macro expansion: create a `let' binding that test definitions
    ;; can pick up on, and create the suite function and suite variable
    `(macrolet ,deftest-macros
       (let ((current-suite ',suite))
         (add-to-list 'all-suites ',suite)
         ;; Add this suite to other suites?
         ,@(when parents
             `((mapc (lambda (suite)
                       (add-to-suite ',suite suite))
                     ',parents)))
         (defvar ,suite nil ,doc)
         (defun ,suite (&optional debug &rest tests)
           ,doc
           (interactive "p")
           (let ((result t)
                 (logged-tests logged-tests)
                 (silent-tests silent-tests)
                 fail-msg own-tests)
             (if (numberp debug)
                 (setq debug (/= debug 0))
               ,@(when debug `((setq debug ,debug))))
             (when (null tests)
               (setq own-tests t
                     tests ,suite)
               (test-message "Test suite `%s' running ..." ',suite))
             (dolist (test tests)
               (setq fail-msg
                     (with-fixtures ,fixture ,setup ,teardown
                       (funcall test (if debug 'debug 'batch))))
               (if (eq fail-msg t)
                   (test-message "%sTest `%s' passed!"
                                 (if own-tests "    " "") test)
                 (test-message "%sTest `%s' failed!"
                               (if own-tests "    " "") test)
                 (test-warning (when (symbolp test) test) ',suite fail-msg)
                 (setq result fail-msg)))
             (when own-tests
               ;; if `silent-tests' is t and the suite is called
               ;; interactively, echo an unlogged summary
               (when (and silent-tests
                          ,(if (version< emacs-version "23")
                               '(called-interactively-p)
                             '(called-interactively-p 'any)))
                 (setq logged-tests nil
                       silent-tests nil))
               (if (eq result t)
                   (test-message "Test suite `%s' passed!" ',suite)
                 (test-message "Test suite `%s' failed!" ',suite)))
             result))
         ;; :wrap function?
         ,@(when wrap
             `((defadvice ,suite (around wrap activate)
                 ,@(if (listp wrap)
                       wrap
                     `((,wrap ad-do-it))))))
         ,@test-forms
         ,@(when run
             `((,suite)))
         ',suite))))

(defun empty-suite (&optional debug &rest tests)
  "Pseudo-suite for suiteless tests.
Tests can call themselves via this suite if not associated with
any other suite."
  (interactive "p")
  (let ((result t) own-tests fail-msg)
    (when (numberp debug)
      (setq debug (/= debug 0)))
    (when (null tests)
      (setq own-tests t
            tests all-tests))
    (dolist (test tests)
      (cond
       ((default-suite test)
        (setq fail-msg (funcall test (and debug t))))
       (t
        (setq fail-msg (funcall test (if debug 'debug 'batch)))))
      (if (eq fail-msg t)
          (test-message "Test `%s' passed!" test)
        (test-message "Test `%s' failed!" test)
        (test-warning (when (symbolp test) test) nil fail-msg)
        (setq result fail-msg)))
    (when (and silent-tests
               (with-no-warnings
                 (if (version< emacs-version "23")
                     (called-interactively-p)
                   (called-interactively-p 'any))))
      (setq logged-tests nil
            silent-tests nil))
    (when own-tests
      (test-message "Test%s %s!"
                    (if (= (length tests) 1) "" "s")
                    (if (eq result t) "passed" "failed")))
    result))

(defun add-to-suite (suite test)
  "Add TEST to SUITE."
  (eval `(defvar ,suite nil))
  ;; Suites are basically hooks.
  (add-hook suite test))

(defmacro with-fixtures (fixture setup teardown &rest body)
  "Run BODY with fixtures.
FIXTURE is a one-argument function with which to run the contents
of BODY; SETUP and TEARDOWN are zero-argument functions to run
before and after. Mocks and stubs are guaranteed to be released."
  (declare (indent defun)
           (debug t))
  (let ((resultvar (make-symbol "result")))
    `(let ((fixture ',fixture)
           (setup ',setup)
           (teardown ',teardown))
       (with-mocks-and-stubs
         (unwind-protect
             (save-excursion
               (let (,resultvar)
                 (when setup
                   (funcall setup))
                 (if fixture
                     (funcall fixture
                              (lambda ()
                                (setq ,resultvar (progn ,@body))))
                   (setq ,resultvar (progn ,@body)))
                 ,resultvar))
           (when teardown
             (funcall teardown)))))))

;;; Test macro: `deftest'

(defmacro deftest (test &rest body)
  "Define a test."
  (declare (indent defun)
           (debug (&define name
                           [&optional symbolp]
                           [&optional stringp]
                           [&rest keywordp sexp]
                           def-body)))
  (let (debug doc keyword lambda run fixture suites setup teardown wrap)
    ;; If TEST is not a name (abbreviated form), move it into BODY.
    ;; (A nil name creates an anonymous function.)
    (unless (symbolp test)
      (setq body (append (list test) body)
            test nil))
    ;; Collect parent suite.
    (when (symbolp (car body))
      (add-to-list 'suites (pop body)))
    ;; Collect docstring.
    (when (stringp (car body))
      (setq doc (pop body)))
    ;; Collect keywords.
    (while (keywordp (car body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :run)
        (setq run (pop body)))
       ((memq keyword '(:suite :suites))
        (if (listp (car body))
            (setq suites (append suites (pop body)))
          (add-to-list 'suites (pop body))))
       ((eq keyword :fixture)
        (setq fixture (pop body)))
       ((eq keyword :setup)
        (setq setup (pop body)))
       ((eq keyword :teardown)
        (setq teardown (pop body)))
       ((memq keyword '(:advice :wrap))
        (setq wrap (pop body)))
       ((eq keyword :debug)
        (setq debug (pop body)))
       (t
        (pop body))))
    (unless (or (symbolp fixture)
                (functionp fixture))
      (setq fixture `(lambda () ,@fixture)))
    (unless (or (symbolp setup)
                (functionp setup))
      (setq setup `(lambda () ,@setup)))
    (unless (or (symbolp teardown)
                (functionp teardown))
      (setq teardown `(lambda () ,@teardown)))
    ;; Create function body.
    (setq lambda
          `(lambda (&optional debug suite)
             ,doc
             (interactive "p")
             (let ((result t)
                   (logged-tests logged-tests)
                   (silent-tests silent-tests))
               ;; if `silent-tests' is t and the test is called
               ;; interactively, echo the result unlogged
               (when (and silent-tests
                          ,(if (version< emacs-version "23")
                               '(called-interactively-p)
                             '(called-interactively-p 'any)))
                 (setq logged-tests nil
                       silent-tests nil))
               (if (numberp debug)
                   (setq debug (/= debug 0))
                 ,@(when debug `((setq debug ,debug))))
               (setq suite
                     (or suite
                         ,@(when test `((default-suite ',test)))
                         (and (boundp 'current-suite) current-suite)
                         'empty-suite))
               ,@(when debug `(setq debug ,debug))
               (cond
                ((eq debug 'batch)
                 (with-fixtures ,fixture ,setup ,teardown
                   (condition-case err
                       (progn ,@body)
                     (error (prog1 nil
                              (setq result (error-message-string err)))))))
                ((eq debug 'debug)
                 (let ((debug-on-error t))
                   (setq result nil)
                   (with-fixtures ,fixture ,setup ,teardown
                     ,@body
                     (setq result t))))
                (t
                 (setq result (funcall suite debug ',test))))
               result)))
    (if (null test)
        `(macrolet ,deftest-macros
           (add-to-list 'all-tests ,lambda)
           (dolist (suite ',suites)
             (add-to-suite suite ,lambda))
           (when (and (boundp 'current-suite) current-suite)
             (add-to-suite current-suite ,lambda))
           ,lambda
           ,@(when run
               `((funcall ,lambda))))
      `(macrolet ,deftest-macros
         (add-to-list 'all-tests ',test)
         (dolist (suite ',suites)
           (add-to-suite suite ',test))
         (when (and (boundp 'current-suite) current-suite)
           (put ',test 'suite current-suite)
           (add-to-suite current-suite ',test))
         (defun ,test ,@(cdr lambda))
         ,@(when wrap
             `((defadvice ,test (around wrap activate)
                 ,@(if (listp wrap)
                       wrap
                     (list wrap)))))
         ,@(when run
             `((,test)))
         ',test))))

(defun test-p (object)
  "Return non-nil if OBJECT is a test."
  (member object all-tests))

(defun default-suite (test)
  "Return the default suite of TEST."
  (when (symbolp test)
    (get test 'suite)))

;; Currently, this produces a warning. Could it produce linkified
;; text in a separate buffer instead? (`with-output-to-temp-buffer')
(defun test-warning (test suite &rest strings)
  "Display warning for TEST in SUITE, consisting of STRINGS.
STRINGS are separated by a newline and a tab."
  (let (doc message)
    (if (and (functionp test)
             (stringp (setq doc (documentation test))))
        (add-to-list 'strings
                     (concat (if test (format "(%s) " test) "")
                             (substring doc 0
                                        (string-match "\n" doc))
                             "\n"))
      (when test
        (add-to-list 'strings (format "In `%s':\n" test))))
    (when (stringp (setq doc (get suite 'variable-documentation)))
      (add-to-list 'strings
                   (substring doc 0 (string-match "\n" doc))))
    (dolist (string strings)
      (when (and (stringp string)
                 (not (string= string "")))
        (if message
            (setq message (concat message "\n\t" string))
          (setq message string))))
    (display-warning (or suite 'test) message)))

(defun test-message (string &rest args)
  "Conditionally echo a message.
If `silent-tests' is t, don't echo the message.
If `logged-tests' is nil, don't log the message
in the *Messages* buffer."
  (let ((message-log-max logged-tests))
    (unless silent-tests
      (apply 'message string args))))

;;; Assertion macro: `defassert'

;; This is hairy. What it does is to implement different ways of
;; calling an assertion macro:
;;
;;     (assert-equal foo bar)
;;     (assert-equal
;;       "Is foo equal to bar?"
;;       foo bar)
;;     (assert-equal
;;       foo bar
;;       baz qux)
;;     (assert-equal
;;       "Is foo equal to bar and baz equal to qux?"
;;       foo bar
;;       baz qux)
;;
;; Assertions must be macros, not functions, to use the unevaluated
;; expressions in the failure report.
(defmacro defassert (name args &rest body)
  "Define an assertion macro.

    (defassert assert-macro (doc arg1 arg2)
      ;; macro expansion
      )

The first argument in the argument list is the docstring.
The rest of the arguments are assumed to be repeatable, that is,
\(assert-macro \"test\" x1 y1 x2 y2 ...), unless a &rest argument
is specified."
  (declare (indent defun)
           (debug (&define name lambda-list
                           [&optional stringp]
                           def-body)))
  (let* (body-var doc-var docstring keyword result shadow)
    (when (stringp (car body))
      (setq docstring (list (pop body))))
    (while (keywordp (car body))
      (setq keyword (pop body))
      (cond
       ((eq keyword :shadow)
        (setq shadow (pop body)))
       (t
        (pop body))))
    (if (memq '&rest args)
        ;; If &rest is specified, just make a `let' wrapper for the
        ;; documentation argument.
        (setq body-var (car (last args))
              args (nbutlast args 2)
              doc-var (or (pop args) 'doc)
              result
              ;; This is an expression for calculating the macro
              ;; definition, which in turn is evaluated to get the
              ;; macro expansion.
              `(let ((,doc-var
                      (when (and (> (length ,body-var) 1)
                                 (stringp (car ,body-var)))
                        (pop ,body-var))))
                 ,@body))
      ;; Repeatable argument list: iterate through the arguments.
      (setq body-var 'body
            doc-var (or (when (> (length args) 1)
                          (pop args)) 'doc)
            result
            ;; Like above, code for making code for making the
            ;; expansion.
            `(let ((result '(progn)) ,doc-var)
               (if (and (> (length ,body-var) 0)
                        ,@(when (> (length args) 1)
                            `((/= (mod (length ,body-var)
                                       ,(length args))
                                  0)))
                        (stringp ,(car args)))
                   ;; Grab first argument as docstring if appropriate,
                   ;; then store the remaining in `body-var' for
                   ;; iteration.
                   (setq ,doc-var ,(car args)
                         ,@(when (cdr args)
                             `(,body-var (append (list ,@(cdr args))
                                                 ,body-var))))
                 (setq ,body-var (append (list ,@args) ,body-var)))
               ;; Go through `body-var' and bind the arguments to
               ;; successive values, evaluating the assertion's
               ;; definition each time. The concatenation of these
               ;; evaluations is the macro's expansion (stored in
               ;; `result').
               (while ,body-var
                 ,@(mapcar (lambda (var)
                             `(setq ,var (pop ,body-var))) args)
                 (add-to-list 'result (progn ,@body) t))
               result)))
    `(progn
       ;; Define assertion macro unless :shadow is t.
       ,@(unless shadow
           `((defmacro ,name (,@args &rest ,body-var)
               ,@docstring
               (declare (indent defun))
               ,result)))
       ;; Add to `deftest-macros' for good measure.
       (eval-and-compile
         (add-to-list 'deftest-macros
                      '(,name (,@args &rest ,body-var) ,result)))
       ',name)))

;; assert { 2.0 }-like display of arguments and their evaluated
;; values. Is this sufficient, or should I use `macrolet' to remap
;; (assert (eq foo bar)) to (assert-eq foo bar)?
(eval-and-compile
  (defun assert-expand (form &optional prefix)
    "Return evalutions for arguments in function call FORM."
    (setq prefix (or prefix ""))
    (apply 'concat
           (mapcar
            (lambda (exp)
              (format "\n%s%s => %s"
                      prefix exp (eval exp)))
            (and (listp form)
                 (and (functionp (car form)))
                 (cdr form))))))

(defassert assert (doc form)
  "Verify that FORM returns non-nil."
  :shadow t
  `(let ((form ,form))
     (without-mocks-and-stubs
       (unless form
         (error "%sassert for %s failed:\n\texpected non-nil, was %s%s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',form form
                ,(assert-expand form "\t"))))))

(defassert assert-not (doc form)
  "Verify that FORM returns nil."
  :shadow t
  `(let ((form ,form))
     (without-mocks-and-stubs
       (when form
         (error "%sassert-not for %s failed:\n\texpected non-nil, was %s%s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',form form
                ,(assert-expand form "\t"))))))

(put 'assert 'lisp-indent-function 'defun)
(put 'assert-not 'lisp-indent-function 'defun)

;; `assert'-derivatives, on the other hand, can be defined
;; straightforwardly.
(defassert assert-that (doc form)
  "Verify that FORM returns non-nil."
  `(let ((form ,form))
     (without-mocks-and-stubs
       (unless form
         (error "%sassert-that for %s failed:\n\texpected non-nil, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',form form)))))

(defassert assert-nil (doc form)
  "Verify that FORM returns nil."
  `(let ((form ,form))
     (without-mocks-and-stubs
       (when form
         (error "%sassert-nil for %s failed:\n\texpected nil, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',form form)))))

;; xUnit discrepancy: xUnit favors Yoda conditions (expected actual).
;; I don't (actual expected).
(defassert assert-equal (doc actual expected)
  "Verify that ACTUAL is `equal' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (equal actual expected)
         (error "%sassert-equal for %s failed:\n\texpected %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-not-equal (doc actual expected)
  "Verify that ACTUAL is not `equal' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (equal actual expected)
         (error "%sassert-not-equal for %s failed:\n\texpected not %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-eq (doc actual expected)
  "Verify that ACTUAL is `eq' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (eq actual expected)
         (error "%sassert-eq for %s failed:\n\texpected %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-not-eq (doc actual expected)
  "Verify that ACTUAL is not `eq' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (eq actual expected)
         (error "%sassert-not-eq for %s failed:\n\texpected not %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-eql (doc actual expected)
  "Verify that ACTUAL is `eql' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (eql actual expected)
         (error "%sassert-eql for %s failed:\n\texpected %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-not-eql (doc actual expected)
  "Verify that ACTUAL is not `eql' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (eql actual expected)
         (error "%sassert-not-eql for %s failed:\n\texpected not %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-= (doc actual expected)
  "Verify that ACTUAL is `=' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (= actual expected)
         (error "%sassert-= for %s failed:\n\texpected %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-/= (doc actual expected)
  "Verify that ACTUAL is `/=' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (/= actual expected)
         (error "%sassert-/= for %s failed:\n\texpected not %s, was %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-string= (doc actual expected)
  "Verify that ACTUAL is `string=' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (unless (string= actual expected)
         (error "%sassert-string= for %s failed:\n\texpected \"%s\", was \"%s\"\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-not-string= (doc actual expected)
  "Verify that ACTUAL is not `string=' to EXPECTED."
  `(let ((actual ,actual)
         (expected ,expected))
     (without-mocks-and-stubs
       (when (string= actual expected)
         (error "%sassert-not-string= for %s failed:\n\texpected \"%s\", was \"%s\"\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',actual expected actual)))))

(defassert assert-member (doc elt list)
  "Verify that ELT is `member' of LIST."
  `(let ((elt ,elt)
         (list ,list))
     (without-mocks-and-stubs
       (unless (member elt list)
         (error "%sassert-member failed:\n\texpected %s in %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                elt list)))))

(defassert assert-memq (doc elt list)
  "Verify that ELT is `memq' of LIST."
  `(let ((elt ,elt)
         (list ,list))
     (without-mocks-and-stubs
       (unless (memq elt list)
         (error "%sassert-memq failed:\n\texpected %s in %s\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                elt list)))))

(defassert assert-match (doc regexp string)
  "Verify that REGEXP matches STRING."
  `(let ((regexp ,regexp)
         (string ,string))
     (without-mocks-and-stubs
       (unless (string-match regexp string)
         (error "%sassert-match failed:\n\texpected \"%s\" to match \"%s\"\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                regexp string)))))

(defassert assert-error (doc &rest body)
  "Verify that BODY signals an error."
  `(let (failed)
     (condition-case nil
         (progn
           ,@body
           (setq failed t))
       (error nil))
     (without-mocks-and-stubs
       (when failed
         (error "%sassert-error failed: %s expected to signal an error\n"
                ,(if (stringp doc) (concat doc "\n\t") "")
                ',body)))))

(defassert assert-changed (doc form &rest body)
  "Verify that FORM changes after executing BODY.
FORM must be quoted."
  `(let* ((form (eval ,form)))
     (assert-not-equal
       ,@(when doc `(,doc))
       (progn
         ,@body
         ,(eval form))
       form)))

(defassert assert-not-changed (doc form &rest body)
  "Verify that FORM does not change after executing BODY.
FORM must be quoted."
  `(let* ((form (eval ,form)))
     (assert-equal
       ,@(when doc `(,doc))
       (progn
         ,@body
         ,(eval form))
       form)))

;;; Mocks/stubs

(defvar stubs nil
  "List of stubbed functions.")

(defvar stubs-global nil
  "List of all stubs in all contexts.")

(defvar mocks-alist nil
  "Alist of mocked functions.")

(defvar mocks-global-alist nil
  "Alist of all mocks in all contexts.")

;; Stubs are made with advice, so the number of arguments is unchanged.
(defmacro stub (func &rest body)
  "Stub out FUNC.
A stub is a temporary function advice shadowing the real
definition (so the argument list is retained). It is released
with `release-stubs'. You should stub inside `deftest' only,
which releases automatically."
  (declare (indent defun)
           (debug t))
  `(cond
    ((fboundp ',func)
     (add-to-list 'stubs ',func nil 'eq)
     (add-to-list 'stubs-global ',func nil 'eq)
     (defadvice ,func (around stub activate)
       (if stubs-global
           (setq ad-return-value
                 (progn ,@(or body '(nil))))
         ad-do-it)))
    (t
     (mock ,func (&rest args)))))

(defun release-stub (func)
  "Release stub for FUNC."
  (condition-case nil
      (progn
        (ad-remove-advice func 'around 'stub)
        (ad-update func))
    (error (release-mock func)))
  (setq stubs (delq func stubs))
  func)

(defun release-stubs (&rest funcs)
  "Release stubs for FUNCS.
Release stubs in `stubs' if unspecified."
  (setq funcs (or funcs stubs))
  (mapc 'release-stub funcs))

(defmacro with-stubs (&rest body)
  "Run BODY, releasing stubs afterwards.
Don't use this directly; see `with-mocks-and-stubs' instead."
  (declare (indent 0)
           (debug t))
  `(let ((stubbed-already stubs-global)
         stubs)
     (unwind-protect
         (progn ,@body)
       (release-stubs)
       (unless stubbed-already
         (setq stubs-global nil)))))

(defmacro without-stubs (&rest body)
  "Run BODY without stubs."
  (declare (indent 0)
           (debug t))
  `(let (stubs-global)        ; stubs are contingent on `stubs-global'
     ,@body))

;; Mocks are temporary function rebindings.
(defmacro mock (func args &rest body)
  "Mock FUNC.
A mock is a temporary function redefinition, released with
`release-mocks'. You should mock inside `deftest' only,
which releases automatically."
  (declare (indent defun)
           (debug t))
  `(mock-fset ',func (lambda ,args ,@(or body '(nil)))))

(defun mock-fset (symbol definition)
  "Mock SYMBOL with DEFINITION.
Don't use this function directly; see `mock' instead."
  (let ((olddef (when (fboundp symbol)
                  (symbol-function symbol)))
        (oldmsg (current-message))
        message-log-max)
    (add-to-list 'mocks-alist (cons symbol olddef))
    (unless (assq symbol mocks-global-alist)
      (add-to-list 'mocks-global-alist (cons symbol olddef)))
    (if definition
        (ad-safe-fset symbol definition)
      (fmakunbound symbol))
    (unless (eq symbol 'message)
      (if oldmsg (message "%s" oldmsg)
        (message nil)))
    symbol))

(defun release-mock (func)
  "Release mock for FUNC."
  (let ((oldmsg (current-message))
        message-log-max orig def)
    (when (setq orig (assq func mocks-alist))
      (setq def (cdr orig))
      (if def
          (ad-safe-fset func def)
        (fmakunbound func))
      (setq mocks-alist (assq-delete-all func mocks-alist))
      (if oldmsg (message "%s" oldmsg)
        (message nil)))
    func))

(defun release-mocks (&rest funcs)
  "Release mocks for FUNCS.
Release mocks in `mocks-alist' if unspecified."
  (setq funcs (or funcs (mapcar 'car mocks-alist)))
  (mapc 'release-mock funcs))

(defmacro with-mocks (&rest body)
  "Run BODY, releasing mocks afterwards.
Don't use this directly; see `with-mocks-and-stubs' instead."
  (declare (indent 0)
           (debug t))
  `(let ((mocked-already mocks-global-alist)
         mocks-alist)
     (unwind-protect
         (progn ,@body)
       (release-mocks)
       (unless mocked-already
         (setq mocks-global-alist nil)))))

(defmacro without-mocks (&rest body)
  "Run BODY without mocks."
  (declare (indent 0)
           (debug t))
  `(with-mocks
     (dolist (func mocks-global-alist)
       (mock-fset (car func) (cdr func)))
     ,@body))

(defmacro with-mocks-and-stubs (&rest body)
  "Run BODY, releasing mocks and stubs afterwards."
  (declare (indent 0)
           (debug t))
  `(with-mocks
     (with-stubs
       ,@body)))

;; This may seem paranoid, but could be useful for "jesting"
;; (where you replace `<' with `>=', `=' with `/=', etc.,
;; run the tests, and weep in despair as they still pass).
;; See http://jester.sourceforge.net/
(defmacro without-mocks-and-stubs (&rest body)
  "Run BODY without mocks and stubs."
  (declare (indent 0)
           (debug t))
  `(without-mocks
     (without-stubs
       ,@body)))

(defalias 'with-stubs-and-mocks 'with-mocks-and-stubs)
(defalias 'without-stubs-and-mocks 'without-mocks-and-stubs)

;;; Highlighting

(when (fboundp 'font-lock-add-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(deftest\\|defsuite\\|defassert\\)\\>[ \f\t\n\r\v]*\\(\\sw+\\)?"
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     ("(\\(assert\\(-[^ ]+\\)*\\|stub\\|mock\\)\\>" 1 font-lock-warning-face)
     ("(\\(with\\(out\\)?-\\(fixtures\\|stubs\\|mocks\\|mocks-and-stubs\\|stubs-and-mocks\\)\\)\\>"
      1 font-lock-keyword-face))))

;;;; Okay, I'm done! Now on to test Vimpulse.

(defun vimpulse-test-buffer (body)
  "Execute BODY in a temporary buffer.
The buffer contains the familiar *scratch* message,
with point at position 1 and in vi (command) state."
  (let ((kill-ring kill-ring)
        (kill-ring-yank-pointer kill-ring-yank-pointer)
        x-select-enable-clipboard
        message-log-max)
    (with-temp-buffer
      (save-window-excursion
        (switch-to-buffer-other-window (current-buffer))
        (buffer-enable-undo)
        (save-excursion
          (insert ";; This buffer is for notes you don't want to save, \
and for Lisp evaluation.\n;; If you want to create a file, visit \
that file with C-x C-f,\n;; then enter the text in that file's own \
buffer.\n"))
        (viper-mode)
        (funcall body)))))

(defsuite test-utils-suite
  "Test suite for vimpulse-utils.el.
This line is not included in the report."
  :run t
  :setup ((require 'vimpulse))
  (test-augment-keymap
   "Test `vimpulse-augment-keymap'."
   (let (augment-alist map)
     (setq map (make-sparse-keymap))
     (define-key map "a" 'foo)
     (define-key map "b" 'bar)
     (define-key map "c" 'baz)
     (setq augment-alist
           '(([?a] . wibble)
             ([?b] . wobble)
             ([?c] . wubble)
             ([?d] . flob)))
     (vimpulse-augment-keymap map augment-alist)
     (assert-eq
       "Augment keymap carefully."
       (lookup-key map "a") 'foo
       (lookup-key map "b") 'bar
       (lookup-key map "c") 'baz
       (lookup-key map "d") 'flob)
     (vimpulse-augment-keymap map augment-alist t)
     (assert-eq
       "Augment keymap forcefully."
       (lookup-key map "a") 'wibble
       (lookup-key map "b") 'wobble
       (lookup-key map "c") 'wubble
       (lookup-key map "d") 'flob)))
  (test-truncate
   "Test `vimpulse-truncate'."
   (assert-equal
     "Positive numbers."
     (vimpulse-truncate [a b c] 0) []
     (vimpulse-truncate [a b c] 1) [a]
     (vimpulse-truncate [a b c] 2) [a b]
     (vimpulse-truncate [a b c] 3) [a b c]
     (vimpulse-truncate [a b c] 4) [a b c])
   (assert-equal
     "Negative numbers."
     (vimpulse-truncate [a b c] -1) [a b]
     (vimpulse-truncate [a b c] -2) [a]
     (vimpulse-truncate [a b c] -3) []
     (vimpulse-truncate [a b c] -4) [])
   (assert-equal
     "Limit cases."
     (vimpulse-truncate [] 0) []
     (vimpulse-truncate [] 3) []))
  (test-filter-list
   "Test `vimpulse-filter-list'."
   (let* ((foo '(a nil b nil c))
          (bar (cdr foo))
          (baz (cdr bar)))
     (assert-equal
       "Filter whole list."
       (vimpulse-filter-list foo 'null) '(a b c))
     (assert-equal
       "Filter first element."
       (vimpulse-filter-list foo 'null bar) '(a nil b nil c))
     (assert-equal
       "Filter first and second element."
       (vimpulse-filter-list foo 'null baz) '(a b nil c)))))

;; These tests are largely interactive (and heavy), so don't run them
;; automatically; add (test-interactive-suite) to .emacs and/or run
;; `M-x test-interactive-suite' manually.
(defsuite test-interactive-suite
  "Interactive test suite for Vimpulse."
  :setup ((require 'vimpulse))
  :fixture vimpulse-test-buffer
  (test-visual-delete-word
   "Visually delete a word."
   (execute-kbd-macro "wved")
   (assert-string=
     (buffer-substring 1 47)
     ";;  buffer is for notes you don't want to save"))

  (test-visual-delete-line
   "Visually delete a line."
   (execute-kbd-macro "Vd")
   (assert-string=
     (buffer-string)
     ";; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.\n"))

  (test-visual-delete-block
   "Visually delete `;; ' prefix."
   (execute-kbd-macro "\C-vjjlld")
   (assert-string=
     (buffer-string)
     "This buffer is for notes you don't want to save, and for \
Lisp evaluation.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer.\n"))

  (test-visual-delete-block-with-counts
   "Visually delete `;; ' prefix, using counts."
   (execute-kbd-macro "\C-v2l2jd")
   (assert-string=
     (buffer-string)
     "This buffer is for notes you don't want to save, and for \
Lisp evaluation.
If you want to create a file, visit that file with C-x C-f,
then enter the text in that file's own buffer.\n"))

  (test-end-of-Word
   "Test E (`vimpulse-end-of-Word')."
   (execute-kbd-macro "wr+d0yEP")
   (assert-string=
     "Yank and put +his before point."
     (buffer-substring 1 52)
     "+his+his buffer is for notes you don't want to save")
   (execute-kbd-macro "EEEwcwfoo")
   (assert-string=
     "Move to \"for\" and change to \"foo\"."
     (buffer-substring 1 52)
     "+his+his buffer is foo notes you don't want to save")
   (execute-kbd-macro "$Ewcw`foo'")
   (assert-string=
     "Move to next line and change \"If\" to \"`foo'\"."
     (buffer-substring 79 113)
     ";; `foo' you want to create a file")
   (execute-kbd-macro "By2EP")
   (assert-string=
     "Yank two words and put before point."
     (buffer-substring 79 122)
     ";; `foo' you`foo' you want to create a file")
   (execute-kbd-macro "EEEEEwcw(bar)")
   (assert-string=
     "Change a single-letter word."
     (buffer-substring 97 126)
     "you want to create (bar) file")
   (execute-kbd-macro "bldiW")
   (assert-string=
     "Delete inner Word."
     (buffer-substring 97 121)
     "you want to create  file"))

  (test-visual-replace
   "Replace with Visual selection."
   (execute-kbd-macro "wvjra")
   (assert-string=
     "Visually replace parts of two lines."
     (buffer-string)
     ";; aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
aaaaf you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.\n")
   (execute-kbd-macro "jjvral.l.")
   (assert-string=
     "Replace a character and repeat for subsequent characters."
     (buffer-substring 141 191)
     ";; aaan enter the text in that file's own buffer.\n")
   (execute-kbd-macro "$ra")
   (assert-string=
     "Replace at end of line."
     (buffer-substring 141 191)
     ";; aaan enter the text in that file's own buffera\n"))

  (test-change-undo
   "Change a word and undo."
   (let ((vimpulse-want-change-undo t))
     (execute-kbd-macro "wcwfoou")
     (assert-string=
       (buffer-substring 1 51)
       ";; This buffer is for notes you don't want to save"))))

(provide 'vimpulse-test)

;;; vimpulse-test.el ends here
