(define-module timer
  (use gauche.record)
  (use gauche.threads)
  (export TIMER_SECOND
          TIMER_MINUTE
          TIMER_HOUR
          TIMER_DAY
          <timer>
          timer?
          timer-state
          timer-repeat
          timer-repeat-set!
          timer-interval
          timer-interval-set!
          timer-proc
          timer-proc-set!
          timer-result
          timer
          timer-start!
          timer-stop!
          timer-terminate!))

(select-module timer)

(define-constant TIMER_SECOND #e1e9)
(define-constant TIMER_MINUTE (* TIMER_SECOND 60))
(define-constant TIMER_HOUR   (* TIMER_MINUTE 60))
(define-constant TIMER_DAY    (* TIMER_HOUR 24))

(define-record-type <timer> make-timer timer?
  (state    timer-state    timer-state-set!)
  (repeat   timer-repeat   timer-repeat-set!)
  (interval timer-interval timer-interval-set!)
  (proc     timer-proc     timer-proc-set!)
  (result   timer-result   timer-result-set!)
  (thread   timer-thread   timer-thread-set!))

(define (make-timer-thread tm)
  (make-thread
   (^[] (let loop ()
          (sys-nanosleep (timer-interval tm))
          (when (timer-state tm)
            (timer-result-set! tm
                               (guard (e [else (timer-result-set! tm e)
                                               (raise e)])
                                 ((timer-proc tm) tm)))
            (when (timer-repeat tm)
              (loop)))))))

(define (timer proc :key (interval TIMER_SECOND) (repeat #t) (start #t))
  (rlet1 tm (make-timer #t repeat interval proc (undefined) (undefined))
    (timer-thread-set! tm (make-timer-thread tm))
    (when start (timer-start! tm))))

(define (timer-start! tm)
  (let* ([th (timer-thread tm)]
         [ts (thread-state th)])
    (cond [(eq? ts 'new)
           (thread-start! th)
           (timer-state-set! tm #t)
           #t]
          [(eq? ts 'stopped)
           (thread-cont! th)
           (timer-state-set! tm #t)
           #t]
          [(eq? ts 'runnable)
           (timer-state-set! tm #t)
           #f]
          [(eq? ts 'terminated)
           (timer-thread-set! tm (thread-start! (make-timer-thread tm)))
           (timer-state-set! tm #t)
           #t])))

(define (timer-stop! tm)
  (timer-state-set! tm #f))

(define (timer-terminate! tm)
  (thread-terminate! (timer-thread tm))
  (timer-state-set! tm #f))
