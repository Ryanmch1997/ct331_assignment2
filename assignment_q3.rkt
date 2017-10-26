(define bst-create-empty   
  (lambda ()
    '()))

(define bst-create         ; constructor
  (lambda (value left-subtree right-subtree)
    (list value left-subtree right-subtree)))

(define bst-isempty? (lambda (BST) (null? BST)))


(define bst-value (lambda (BST) (first BST)))
(define bst-left-subtree (lambda (BST) (first (rest BST))))
(define bst-right-subtree (lambda (BST) (first (rest (rest BST)))))
(define bst-traverse-inorder  
  (lambda (BST)                 
    (cond                      
      ((bst-isempty? BST) '()) 
      (else (append
              (bst-traverse-inorder (bst-left-subtree BST))
              (list (bst-value BST)) ; each argument to append must be a list
              (bst-traverse-inorder (bst-right-subtree BST)))))))
(define bst-add    ; return tree with value added
  (lambda (BST value)
    (cond
      ((bst-isempty? BST)           ; if empty, create a new node
       (bst-create value (bst-create-empty) (bst-create-empty))) 
      ((< value (bst-value BST))    ; add node to left subtree 
       (bst-create (bst-value BST)  ; (functionally, by building new tree)
                   (bst-add (bst-left-subtree BST) value)
                   (bst-right-subtree BST)))
      ((> value (bst-value BST))    ; add node to right subtree
       (bst-create (bst-value BST)
                   (bst-left-subtree BST)
                   (bst-add (bst-right-subtree BST) value)))
      (else BST))))     