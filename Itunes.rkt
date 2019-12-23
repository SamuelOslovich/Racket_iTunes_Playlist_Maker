#lang racket
(require 2htdp/itunes)
(require racket/gui/base)
(require 2htdp/batch-io)
(require "TrackHeap.rkt")

;Creates a new trackheap object to hold the tracks
(define heap (new trackheap%))


;A list that stores all of the tracks in the users library
(define track-list '())


;Inserts all tracks in a list into a trackheap
(define (insert-into-heap TL Heap)
  (cond ((null? TL) 0)
        (else (begin (send Heap insert-heap (car TL))
                     (insert-into-heap (cdr TL) Heap)))))


;Keeps track if the user has generated their top ten playlist yet
(define generatedtopten? #f)


;Returns the top ten most played songs as a text file
(define (toptenplayed string acc)
  (cond (generatedtopten? "")
        ((= acc 10) (begin (set! generatedtopten? #t)
                           (write-file "TopTenPlaylist.txt" string)))
        ((send heap empty?) (write-file "TopTenPlaylist.txt" string))
        (else (toptenplayed (string-append string
                                           (car (send heap get-and-extract))
                                           "\n")
                            (+ acc 1)))))


;Keeps track if the user has generated their only played once playlist yet
(define generatedsingle? #f)


;Returns all the songs only listened to once as a text file
(define (singleplayed string tracklist)
  (cond (generatedsingle? "")
        ((null? tracklist) (write-file "SinglePlayedPlaylist.txt" string))
        ((= (track-play# (car tracklist)) 1) (singleplayed (string-append string
                                                                          (track-name (car tracklist))
                                                                          "\n")
                                                           (cdr tracklist)))
        (else (singleplayed string (cdr tracklist)))))


;Returns a random playlist of songs with a 25% chance for each song to be include
(define (randomplaylist string tracklist)
  (cond ((null? tracklist) (write-file "RandomPlaylist.txt" string))
        ((= 1 (random 0 5)) (randomplaylist (string-append string
                                                           (track-name (car tracklist))
                                                           "\n")
                                            (cdr tracklist)))
        (else (randomplaylist string (cdr tracklist)))))


;The maximum number of times any song has been played
(define maxplays 1)


;Returns a random playlist of songs with a 50% chance for the
;most played song to be included and a larger percentage for the
;songs that have been listened to less
(define (randomweightedplaylist string tracklist)
  (cond ((null? tracklist) (write-file "RandomWeightedPlaylist.txt" string))
        ((< (track-play# (car tracklist)) (random 0 (+ 1 (* 2 maxplays)))) (randomweightedplaylist (string-append string
                                                                                                                   (track-name (car tracklist))
                                                                                                                   "\n")
                                                                                                    (cdr tracklist)))
        (else (randomweightedplaylist string (cdr tracklist)))))


;Creates the window
(define frame (new frame% [label "Itunes Playlist Maker"]))


;When the button is clicked the user is prompted for their itunes library.xml file
(define filebutton (new button% [parent frame]
                        [label "Select iTunes Library"]
                        [callback (lambda (button event)
                                    (begin (define library (get-file))
                                           (set! track-list (read-itunes-as-tracks library))
                                           (insert-into-heap track-list heap)
                                           (set! maxplays (cdr (send heap get-heap-root))))
                                    )]))


;Gets the top ten most played songs in the library
(define mostplayedbutton (new button% [parent frame]
                              [label "Generate Top Ten Most Played Songs"]
                              [callback (lambda (button event)
                                          (toptenplayed "" 0)
                                          )]))


;Gets all the songs that have only been played once in the library
(define singleplayedbutton (new button% [parent frame]
                                [label "Generate All The Songs You Have Only Listened To Once"]
                                [callback (lambda (button event)
                                            (singleplayed "" track-list)
                                            )]))


;Gets a random assortment of songs
(define randombutton (new button% [parent frame]
                          [label "Generate A Random Playlist"]
                          [callback (lambda (button event)
                                      (randomplaylist "" track-list)
                                      )]))


;Gets a random assortment of songs with more weight being
;placed on songs with a higher play number
(define randomweightedbutton (new button% [parent frame]
                                  [label "Generate A Random Weighted Playlist"]
                                  [callback (lambda (button event)
                                              (randomweightedplaylist "" track-list)
                                              )]))


;Shows the frame
(send frame show #t)
