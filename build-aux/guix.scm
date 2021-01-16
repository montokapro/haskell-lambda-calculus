;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment --pure --load=build-aux/guix.scm -- ghci

(use-modules
 (guix)
 (guix git-download)
 (guix licenses)
 (guix build-system haskell)
 (gnu packages haskell-check)
 (gnu packages haskell-xyz)
 ((ice-9 popen) #:select (open-pipe))
 ((ice-9 rdelim) #:select (read-string))
 (srfi srfi-1))

(define %git-commit
  (read-string
   (open-pipe "git show HEAD | head -1 | cut -d ' ' -f 2" OPEN_READ)))

(define %aux-dir (current-source-directory))

(define %srcdir (dirname %aux-dir))

(define (keep-file? file stat)
  (not (any (lambda (str) (string-contains file str))
            '(".git" "build-aux"))))

(define-public ghc-lc
  (package
    (name "ghc-lc")
    (version (git-version "0" "HEAD" %git-commit))
    (source
     (local-file
      %srcdir
      #:recursive? #t
      #:select? keep-file?))
    (build-system haskell-build-system)
    (inputs
     `(("ghc-megaparsec" ,ghc-megaparsec)))
    (native-inputs
     `(("ghc-hspec" ,ghc-hspec)))
    (home-page "TODO")
    (synopsis "Exploration of the lambda calculus")
    (description "Exploration of the lambda calculus")
    (license cc0)))

ghc-lc
