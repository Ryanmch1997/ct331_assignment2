(define (ins_beg lst element)
  (set! lst (cons element (list lst)))
  lst)
(define (ins_end element lst)
  (set! lst (append lst (list element)))
  lst)
(define (Cout_top_level list)
  (if (null? list)
      0
      (+ 1 (Cout_top_level (cdr list)))
  )
)

(define (count_instances_tr lst)
  (cond ((null? lst) 0)                  
        ((not (pair? lst)) 1)            
        (else (+ (count_instances (car lst))     
                 (count_instances (cdr lst))))))
(define (count_instances x L)
			(if (null? L)
				0
				(if (eq? x (car L))
					(+ 1 (count_instances x (cdr L)))
					(count_instances x (cdr L)))))




