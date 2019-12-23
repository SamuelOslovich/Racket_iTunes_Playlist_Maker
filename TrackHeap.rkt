#lang racket
(require 2htdp/itunes)

(provide trackheap%)

(define trackheap%
  (class object%
    (init)
    (super-new)

    (define heap '())
    
    ;Creates a new heap
    (define (create-heap value left right)
      (list value left right))

    ;Gets the root of the heap
    (define (heap-root H) (car H))

    ;Gets the left subheap
    (define (left H) (cadr H))

    ;Gets the right subheap
    (define (right H) (caddr H))

    
    ;Weight is represented by number of plays
    
    ;Gets the weight of the root of the heap
    (define (heap-weight H) (track-play# (car H)))

    ;Gets the weight of a track
    (define (weight track) (track-play# track))

    ;Inserts a track into the heap
    (define (heap-insert track H)
      (cond ((null? H) (create-heap track '() '()))
            ((> (weight track) (heap-weight H)) (create-heap track
                                                             (right H)
                                                             (heap-insert (heap-root H) (left H))))
            (else (create-heap (heap-root H)
                               (right H)
                               (heap-insert track (left H))))))

    ;Combines two heaps
    (define (combine Ha Hb)
      (cond ((null? Ha) Hb)
            ((null? Hb) Ha)
            ((> (heap-weight Ha) (heap-weight Hb)) (create-heap (heap-root Ha)
                                                            (combine (left Ha) (right Ha))
                                                            Hb))
            (else (create-heap (heap-root Hb)
                               (combine (left Hb) (right Hb))
                               Ha))))

    ;Removes the root of the heap and returns the remaining 
    ;two heaps combined
    (define (heap-remove H)
      (cond ((null? H) '())
            (else (combine (left H) (right H)))))
    
    
    ;Returns a pair containg the name and plays
    ;of the root of the heap
    (define/public (get-heap-root)
      (if (null? heap)
          (display "The heap is empty")
          (let ((track (heap-root heap)))
            (cons (track-name track)
                  (track-play# track)))))
    
    ;Inserts a track into the heap
    (define/public (insert-heap track)
      (set! heap (heap-insert track heap)))

    ;Returns a pair containg the name and
    ;plays of the root of the heap and then
    ;removes the root of the heap, combining
    ;the remaining two heaps
    (define/public (get-and-extract)
      (if (null? heap)
            (display "The heap is empty")
            (let ((track (heap-root heap)))
              (begin (set! heap (heap-remove heap))
                     (cons (track-name track)
                           (track-play# track))))))

    ;Returns true if the heap is empty
    (define/public (empty?)
      (null? heap))
    ))
