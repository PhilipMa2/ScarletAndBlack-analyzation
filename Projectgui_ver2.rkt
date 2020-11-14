#lang racket/gui
(require racket/include)
(require (file "C:\\Users\\ma_ph\\Documents\\Grinnell\\151Final\\FINAL_ver3.0.rkt")) ;don't forget to change this to your location
(require 2htdp/image)



(define year-list (list "1961" "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970"))
;;; Procedure:
;;;   action
;;; Parameters:
;;;   [no additional]
;;; Purpose:
;;;   generate a graphical user interface
;;; Produces:
;;;   gui, a gui
(define action
  (lambda ()
    (let* ([choice (get-choices-from-user "visualization" "Please select one visualization" (list "Histogram by month" "Histogram by year" "Line plot" "Word cloud"))]
          [frame (new frame% [label "S&B Sentiment Database"]
                      [width 410]
                      [height 300])]
          [canvas (new canvas% [parent frame]
                       [paint-callback
                        (lambda (canvas dc)
                          (send dc set-scale 1 1)
                          (send dc set-text-foreground "olive")
                          (send dc draw-text "Welcome to S&B Sentiment Analysis Database v1.0" 0 0))])]
          [panel (new vertical-panel% [parent frame])]
          [range (new horizontal-panel% [parent panel])])
      (cond
        [(= (car choice) 0)
         (define word (new text-field%
                           (label "Word")
                           (parent panel)
                           (init-value "Interesting Word")))
         (define start (new choice%
                            (label "Start")
                            (parent range)
                            (choices year-list)))
         (define end (new choice%
                          (label "End")
                          (parent range)
                          (choices year-list)))
         (new button%
              (parent panel)
              (label "Get histogram by month")
              (callback (lambda (button event)
                          (let* ([input (string-downcase (send word get-value))]
                                 [yearstart (string->number (list-ref year-list (send start get-selection)))]
                                 [yearend (+ 1 (string->number (list-ref year-list (send end get-selection))))])
                            (print (string-append "Please wait, while we analyze what students had to say about " input " in the S&B..."))
                            (println " ")
                            (display (scbhistogram input yearstart yearend 1))
                            (continue?)))))
         (send frame show #t)]
        [(= (car choice) 1)
         (define word (new text-field%
                           (label "Word")
                           (parent panel)
                           (init-value "Interesting Word")))
         (define start (new choice%
                            (label "Start")
                            (parent range)
                            (choices (list "1961" "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970"))))
         (define end (new choice%
                          (label "End")
                          (parent range)
                          (choices (list "1961" "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970"))))
         (new button%
              (parent panel)
              (label "Get histogram by year")
              (callback (lambda (button event)
                          (let* ([input (string-downcase (send word get-value))]
                                 [yearstart (string->number (list-ref year-list (send start get-selection)))]
                                 [yearend (+ 1 (string->number (list-ref year-list (send end get-selection))))])
                            (print (string-append "Please wait, while we analyze what students had to say about " input " in the S&B..."))
                            (println " ")
                            (display (scbhistogram input yearstart yearend 2))
                            (continue?)))))
         (send frame show #t)]
        [(= (car choice) 2)
         (define word (new text-field%
                           (label "Word")
                           (parent panel)
                           (init-value "Interesting Word")))
         (define start (new choice%
                            (label "Start")
                            (parent range)
                            (choices (list "1961" "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970"))))
         (define end (new choice%
                          (label "End")
                          (parent range)
                          (choices (list "1961" "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970"))))
         (new button%
              (parent panel)
              (label "Get line plot")
              (callback (lambda (button event)
                          (let* ([input (string-downcase (send word get-value))]
                                 [yearstart (string->number (list-ref year-list (send start get-selection)))]
                                 [yearend (+ 1 (string->number (list-ref year-list (send end get-selection))))])
                            (print (string-append "Please wait, while we analyze what students had to say about " input " in the S&B..."))
                            (println " ")
                            (display (scblineplot input yearstart yearend))
                            (continue?)))))
         (send frame show #t)]
        [(= (car choice) 3)
         (define word (new text-field%
                           (label "Word")
                           (parent panel)
                           (init-value "Interesting Word")))
         (define start (new choice%
                            (label "Start")
                            (parent range)
                            (choices (list "1961" "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970"))))
         (define end (new choice%
                          (label "End")
                          (parent range)
                          (choices (list "1961" "1962" "1963" "1964" "1965" "1966" "1967" "1968" "1969" "1970"))))
         (new button%
              (parent panel)
              (label "Get wordcloud")
              (callback (lambda (button event)
                          (let* ([input (send word get-value)]
                                 [yearstart (string->number (list-ref year-list (send start get-selection)))]
                                 [yearend (+ 1 (string->number (list-ref year-list (send end get-selection))))])
                            (print (string-append "Please wait, while we analyze what students had to say about " input " in the S&B..."))
                            (println " ")
                            (display (list->img (wordpos input yearstart yearend)))
                            (continue?)))))
         (send frame show #t)]))))

;;; Procedure:
;;;   continue?
;;; Parameters:
;;;   [no additional]
;;; Purpose:
;;;   ask user if he/she want to continue analyzing
;;; Produces:
;;;   prompt 
(define continue?
  (lambda ()
    (let ([choice2 (get-choices-from-user "Continue?" "Do you want to continue analyzing?" (list "Yes" "No"))])
      (cond
        [(= (car choice2) 0)
         (action)]
        [else
         (void)]))))
  
          

(action)
