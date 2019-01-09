(install-special-form 'quote                 eval-quote)
(install-primitive-procedure '= =)
(install-primitive-procedure 'newline newline)
(install-special-form 'define                eval-definition)
(install-special-form 'if                    eval-if)
(install-special-form 'set!                  eval-assignment)
(install-special-form 'lambda                eval-lambda)
(install-special-form 'begin                 eval-begin)
(install-special-form 'cond                  eval-cond)
(install-special-form 'defined?              eval-defined?)
(install-special-form 'locally-defined?      eval-locally-defined?)
(install-special-form 'make-unbound!         eval-make-unbound!)
(install-special-form 'locally-make-unbound! eval-locally-make-unbound!)
(install-special-form 'cons-stream           cons-stream)
(install-primitive-procedure 'stream-car stream-car)
(install-primitive-procedure 'stream-cdr stream-cdr)
(install-primitive-procedure 'stream-null? stream-null?)
(install-primitive-procedure 'the-empty-stream the-empty-stream)
(install-primitive-procedure 'car car)
(install-primitive-procedure 'cdr cdr)
(install-primitive-procedure 'cons cons)
(install-primitive-procedure 'null? null?)
(install-primitive-procedure '+ +)
(install-primitive-procedure '* *)
(install-primitive-procedure 'display display)
(install-primitive-procedure 'list list)
(install-primitive-procedure 'abs abs)
(install-primitive-procedure '/ /)
(install-primitive-procedure '< <)
(install-primitive-procedure '- -)
