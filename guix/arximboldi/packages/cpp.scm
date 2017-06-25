(define-module (arximboldi packages cpp)
  #:use-module (gnu packages)
  #:use-module (gnu packages gcc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (guix licenses))

(define-public libstdc++-6
  (make-libstdc++ gcc-6))
