(use gauche.test)
(use gauche.threads)

(define *n*     0)
(define *mutex* (make-mutex))
(define *cv*    (make-condition-variable))
(define *timer* #f)

(test-start "timer")

(use timer)
(test-module 'timer)

(test* "TIMER_SECOND" 1000000000 TIMER_SECOND)
(test* "TIMER_MINUTE" 60000000000 TIMER_MINUTE)
(test* "TIMER_HOUR" 3600000000000 TIMER_HOUR)
(test* "TIMER_DAY" 86400000000000 TIMER_DAY)

(test* "timer" <timer>
       (begin (set! *timer*
                    (timer (^[t]
                             (inc! *n*)
                             (condition-variable-broadcast! *cv*)
                             "foo")
                           :interval #e1e8
                           :start #f))
              (class-of *timer*)))

(test* "timer-start!" #t
       (begin (timer-start! *timer*)
              (mutex-unlock! *mutex* *cv*)
              (and (> *n* 0)
                   (eq? #t (timer-state *timer*)))))

(test* "timer-stop!" #t
       (begin (timer-stop! *timer*)
              (let1 n *n*
                (mutex-unlock! *mutex* *cv* 1)
                (and (= n *n*)
                     (eq? #f (timer-state *timer*))))))

(test* "timer-start!" #t
       (begin (timer-start! *timer*)
              (mutex-unlock! *mutex* *cv*)
              (and (> *n* 0)
                   (eq? #t (timer-state *timer*)))))

(test* "timer-terminate!" #t
       (begin (timer-terminate! *timer*)
              (let1 n *n*
                (mutex-unlock! *mutex* *cv* 1)
                (and (= n *n*)
                     (eq? #f (timer-state *timer*))))))

(test* "timer-result" "foo"
       (timer-result *timer*))

(test-end)
