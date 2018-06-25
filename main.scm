
(define (count-items alist)
  (cond ((null? alist) 0)
        (#t (+ 1 (count-items (cdr alist))))))

(define (nth-item n alist)
  (cond ((> n (count-items alist)) (display "Not a valid test case"))
        ((= n 1) (car alist))
        (#t (nth-item (- n 1) (cdr alist)))))

(define (exists a alist)
  (cond ((null? alist) #f)
        ((= (count-items alist) 1) (if (equal? a (car alist)) #t #f))
        (#t (if (equal? a (car alist)) #t (exists a (cdr alist))))))

(define (abs value)
  (if (> value 0) value (- 0 value)))

;(define percept '((empty empty empty)(empty empty empty empty empty)(empty empty empty empty empty empty empty)(empty empty empty empty empty empty empty empty empty)((vegetation 0 100) empty empty empty empty (vegetation 0 200) empty empty empty empty (predator 1))))


(define VEGx '()) ;to store the locations of vegetation

;called from the main fucntion below to calculate x coordinate of vegetation
(define (get-veg-x xlist i y)    ;i is iterator, must be 0
  (cond ((null? xlist) (set! VEGx (append VEGx '())))
        ((= (count-items xlist) 1) (if (list? (car xlist))
                                       (if (equal? 'vegetation (car (car xlist)))
                                           (set! VEGx (append VEGx (list (list i y (car (cdr (cdr (car xlist))))))))
                                           (set! VEGx (append VEGx '())))
                                       (set! VEGx (append VEGx '()))))
        (#t (begin (if (list? (car xlist))
                       (if (equal? 'vegetation (car (car xlist))) (set! VEGx (append VEGx (list (list i y (car (cdr (cdr (car xlist)))))))) (set! VEGx (append VEGx '())))
                       (set! VEGx (append VEGx '())))
                   (get-veg-x (cdr xlist) (+ i 1) y)))))
                                   
;main fucntion to get the location of vegetation from the percept and store it in a list named x                                 
(define (get-veg location i)     ;i is iterator, must be 1 for y axis
  (cond ((null? location) #f)
        ((= (count-items location) 1) (get-veg-x (car location) (- 0 i) i))
        (#t (begin (get-veg-x (car location) (- 0 i) i) (get-veg (cdr location) (+ i 1))))))

;get the distances of the vegetation from the agent based on its x and y coordinates 
(define (get-veg-dist vlist)
  (cond ((null? vlist) vlist)
        ((null? (cdr vlist)) (list (cons (+ (abs (car (car vlist))) (car (cdr (car vlist)))) (car vlist))))
        (#t (append (list (cons (+ (abs (car (car vlist))) (car (cdr (car vlist)))) (car vlist))) (get-veg-dist (cdr vlist))))))

;to filter all those vegetation from the list which are below energy 5
(define (filter-veg vlist)
  (cond ((null? vlist) vlist)
        ((null? (cdr vlist)) (if (> (car (cdr (cdr (car vlist)))) 5) vlist '()))
        (#t (append (if (> (car (cdr (cdr (car vlist)))) 5) (list (car vlist)) '()) (filter-veg (cdr vlist))))))

;get the vegetation from the list which is at the shortest distance
(define (get-shortest-veg vlist)
  (cond ((null? vlist) vlist)
        ((null? (cdr vlist)) (car vlist))
        ((null? (cddr vlist)) (if (< (car (car vlist)) (car (car (cdr vlist)))) (car vlist) (car (cdr vlist))))
        (#t (if (< (car (car vlist)) (car (car (cdr vlist))))
                (get-shortest-veg (cons (car vlist) (cddr vlist)))
                (get-shortest-veg (cons (car (cdr vlist)) (cddr vlist)))))))




(define PREx '()) ;to store the locations of predators

;called from the main fucntion below to calculate x coordinate of predators
(define (get-pre-x xlist i y)    ;i is iterator, must be 0
  (cond ((null? xlist) (set! PREx (append PREx '())))
        ((= (count-items xlist) 1) (if (list? (car xlist))
                                       (if (equal? 'predator (car (car xlist)))
                                           (set! PREx (append PREx (list (list i y))))
                                           (set! PREx (append PREx '())))
                                       (set! PREx (append PREx '()))))
        (#t (begin (if (list? (car xlist))
                       (if (equal? 'predator (car (car xlist))) (set! PREx (append PREx (list (list i y)))) (set! PREx (append PREx '())))
                       (set! PREx (append PREx '())))
                   (get-pre-x (cdr xlist) (+ i 1) y)))))
                                   
;main fucntion to get the location of vegetation from the percept and store it in a list named x                                 
(define (get-pre location i)     ;i is iterator, must be 1 for y axis
  (cond ((null? location) #f)
        ((= (count-items location) 1) (get-pre-x (car location) (- 0 i) i))
        (#t (begin (get-pre-x (car location) (- 0 i) i) (get-pre (cdr location) (+ i 1))))))

(define (match-x x1 x2)   ;inner function for dinding if a predator exists in the path
  (cond ((= x1 0) #t)
        ((> x1 0) (if (> x2 0) (if (> x2 x1) #t #f) #t))
        (#t (if (< x2 0) (if (> x1 x2) #t #f) #t))))


(define (trace-path vloc ploc)   ;to find if a given predator location exists in the path to a vegetation location
  (cond ((null? ploc) #t)
        (#t (if (< (car (cdr (cdr vloc))) (car (cdr ploc)))
                                 #t
                                 (if (= 0 (car ploc))
                                     #f
                                     (if (= (car (cdr (cdr vloc))) (car (cdr ploc))) (match-x (car (cdr vloc)) (car ploc)) #t))))))

(define (path-exists? vloc plist)   ;to find if a given list of predators exists in the path to a vegetation location
  (cond ((null? plist) #t)
        ((null? (cdr plist)) (trace-path vloc (car plist)))
        (#t (if (trace-path vloc (car plist)) (path-exists? vloc (cdr plist)) #f))))
                
(define locked-veg '()) ;the target vegetation location

;to remove an item from a list
(define (remove a l1)
  (cond ((null? a) l1)
        ((equal? a (car l1)) (cdr l1))
        (#t (cons (car l1) (remove a (cdr l1))))))

;to find out the current shortest distance vegetation location where the agent needsto be directed
(define (find-cur-veg VEGx PREx)
  (begin (if (null? VEGx) locked-veg (set! locked-veg (get-shortest-veg VEGx)))
         (cond ((null? PREx) locked-veg)
               ((null? VEGx) locked-veg)
               (#t (if (path-exists? locked-veg PREx) locked-veg (begin (set! VEGx (remove locked-veg VEGx)) (find-cur-veg VEGx PREx)))))))

;set the x and y variables with x and y coordinates of locked-veg
(define x 0)
(define y 0)
(define turned #f)
(define (setxy vloc)
  (cond ((null? vloc) (begin (set! x 0) (set! y 0)))
        (#t (begin (set! x (car (cdr vloc))) (set! y (car (cddr vloc)))))))

;movement algo
(define (decrement coordinate)
  (cond ((equal? 'yaxis coordinate) (set! y (- y 1)))
        ((equal? 'xaxis coordinate) (set! x (- x 1)))))

;function to decide what move to make according to the x and y coordinate and the variable turned
(define (move locked-veg)
  (cond ((null? locked-veg) (begin (set! turns (+ 1 turns)) 4))
        ((= x 0) (cond ((> y 1) (begin (set! y (- y 1)) 1))
                       (#t 7)))
        ((> y 0) (begin (set! y (- y 1)) 1))
        ((= y 0) (if (not turned)
                     (cond ((> x 0) (begin (set! x (- x 1)) (set! turned #t) 4))
                                    ((= x 0) (set! locked-veg '()) 7)
                                    ((< x 0) (begin (set! x (+ x 1)) (set! turned #t) 5)))
                     (cond ((> x 0) (begin (set! x (- x 1)) 1))
                                    ((= x 0) (set! locked-veg '()) 7)
                                    ((< x 0) (begin (set! x (+ x 1)) 1)))))))

;function to convert the return values of above functions into actual commands
(define (commands c)
  (cond ((= c 1) "MOVE-PASSIVE-1")
        ((= c 2) "MOVE-PASSIVE-2")
        ((= c 3) "MOVE-PASSIVE-3")
        ((= c 4) "TURN-RIGHT")
        ((= c 5) "TURN-LEFT")
        ((= c 6) "TURN-AROUND")
        ((= c 7) "EAT-PASSIVE")))

(define turns 0) ;to define number of continuos turns it makes before going at a particular direction (of it diesnt find vegeation in all four directions, it takes a step towards one direction)

;constructor; to send the variables back to their initial values
(define (constructor)
  (begin (set! VEGx '()) (set! PREx '()) (set! locked-veg '()) (set! x 0) (set! y 0) (set! turned #f)))

;perform initial set of activities for initializing the current vegetation which is to be targeted 
(define (find&move-veg percept)
  (begin (get-veg percept 1)
         (set! VEGx (get-veg-dist (filter-veg VEGx)))
         (get-pre percept 1)
         (find-cur-veg VEGx PREx)
         (setxy locked-veg)
         (commands (move locked-veg))))

;main function
(define (choose-action energy events percept)
  (cond ((list? (car (cdr (car percept)))) (cond ((equal? (car (car (cdr (car percept)))) 'vegetation)
                                                  (if (= 0 (car (cddr (car (cdr (car percept)))))) (begin (constructor) (commands 4))
                                                                                                   (begin (constructor) (commands 7))))
                                                 ((equal? (car (car (cdr (car percept)))) 'predator) (begin
                                                                                                       (constructor)
                                                                                                       (set! turns (+ turns 1))
                                                                                                       (commands 4)))
                                                 ((equal? (car (car (cdr (car percept)))) 'agent) (begin (constructor) (commands 4)))))
        ((and (null? locked-veg) (null? events)) (begin (constructor) (find&move-veg percept)))
        ((equal? (car (cdr (car percept))) 'barrier) (begin (constructor) (set! turns (+ 1 turns)) (commands 4)))
        ((not (null? locked-veg)) (cond ((and (null? events) (equal? turned #t)) (commands (move locked-veg)))
                                        ((and (null? events) (equal? turned #f)) (begin (constructor) (commands 4)))
                                        (#t (commands (move locked-veg)))))
        ((null? locked-veg) (cond ((and (null? events) (equal? turned #f) (< turns 4)) (begin (constructor) (set! turns (+ turns 1)) (commands 4)))
                                        ((and (null? events) (equal? turned #f) (> turns 3)) (begin (constructor) (set! turns 0) (commands 1)))
                                        ((and (equal? turned #f) (> turns 3)) (begin (constructor) (set! turns 0) (commands 1)))
                                        ((and (equal? turned #f) (< turns 4)) (begin (constructor) (set! turns (+ turns 1)) (find&move-veg percept)))
         (#t (begin (constructor) (find&move-veg percept)))))))

(define (initialize-agent)
        "OK")
