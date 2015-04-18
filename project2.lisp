(setq p1 '(and x (or x (and y (not z)))))
(setq p2 '(and (and z nil) (or x 1)))
(setq p3 '(or 1 a))

(defun andexp (e1 e2) (list 'and e1 e2))
(defun orexp  (e1 e2) (list 'or e1 e2))
(defun notexp (e1) (list 'not e1))

(setq sAnd '(and))
(setq sOr '(or))
(setq sNot '(not))

(setq bindings '((x nil) (z 1)))
(setq binding1 '(X 1))
(setq binding2 '(A nil))

(setq c1 '(and 1 (and (not 1) (or x y)))) ;;good
(setq c2 '(and 1 (and (not 0) (or x y)))) ;;good
(setq c3 '(and (not (not 1)) (or x y)));;good

(setq n1 '(not 1))
(setq n2 '(not (not 1)))
(setq n3 '(not (not (not (not 1)))))
(setq n31 '(not (not (not (not x)))))
(setq n32 '(not (not (not x))))
(setq n4 '(not (and x y)))
(setq n5 '(not (or x y)))

(setq a1 '(and 0 1)) ;; 0 good
(setq a12 '(and 1 0)) ;;0 good
(setq a13 '(and 1 1)) ;;1 good
(setq a2 '(and (and 1 1) (and 1 1))) ;;1 good
(setq a3 '(and 1 (and 1 (and 1 0)))) ;;0 good
(setq a4 '(and 1 (and x (and 1 1)))) ;;x good
(setq a41 '(and 1 (and x (and x 1)))) ;;x good
(setq a42 '(and 1 (and x (and x 0)))) ;; 0 good
(setq a43 '(and 1 (and x (and x nil))));;0 good
(setq a44 '(and 1 (and x (and x y)))) ;; (AND X (AND X Y)) good
(setq a45 '(and x (and x y))) ;;;(AND X (AND X Y)) good

(setq o1 '(or 0 1));; ==> 1 good
(setq o12 '(or 1 0));; ==> 1 good
(setq o13 '(or 1 1));; ==> 1 good
(setq o2 '(or (or 1 1) (or 1 1)));; ==> 1 good
(setq o3 '(or 1 (or 1 (or 1 0))));; ==> 1 good
(setq o4 '(or 1 (or x (or 1 1))));; ==> 1 good
(setq o41 '(or 1 (or x (or x 1))));; ==> 1 good
(setq o42 '(or 1 (or x (or x 0))));; ==> 1 good
(setq o43 '(or 1 (or x (or x nil))));; ==> 1 good
(setq o44 '(or 1 (or x (or x y))));;  ==> 1 good
(setq o45 '(or x (or x y)));; ==> (or x (or x y))

(defun evalexp (bindings l) (simplify (simplify (bind-first bindings l))))

(defun bind-first (bindings l)
	(cond
	((null l) nil)
	((null bindings) l)
	((atom (car bindings)) nil)
	(t (bind-first (cdr bindings)(simplebind (car bindings) l)))))

(defun simplebind (binding l)
	(cond
	((null l) nil)
	((null binding) l)
	((atom binding) l)
	((atom l)
		(cond
		((atom (car binding))
			(cond
			((equal (car binding) l) (cons (car (cdr binding))))
			(t l)))
		(t l)))
	((atom (car binding))
		(cond
		((equal (car binding)(car l)) (cons (car (cdr binding))(simplebind binding (cdr l))))
		(t (cons (simplebind binding (car l)) (simplebind binding (cdr l))))))
	(t l)))

(defun simplify (l)
	(cond
	((null l) l)
	((atom l) l)
	((atom (car l))
		(cond
		((equal (car l) (car sNot))(notSimplify (cdr l)))
		((equal (car l) (car sAnd))(andSimplify (cdr l)))
		((equal (car l) (car sOr))(orSimplify (cdr l)))
		(t l)))
	(t (simplify (cons (simplify(car l)) (simplify (cdr l)))))))

(defun orSimplify (l)
	(cond  
	((null l) l)
	((atom (car l))
		(cond
		((equal (car l) 0) (simplify (simplify (car (cdr l)))))
		((equal (car l) nil) (simplify (simplify (car (cdr l)))))
		((equal (car l) 1) 1)
		(t
			(cond
			((atom (car (cdr l)))
				(cond
				((equal (car (cdr l)) 0) (simplify (car l)))
				((equal (car (cdr l)) nil) (simplfy (car l)))
				((equal (car (cdr l)) 1) 1)
				((equal (car (cdr l)) (car l)) (simplify (car l)))
				(t (cons (car sOr) (simplify l)))))
			(t (list (car sOr) (simplify (car l)) (simplify (simplify (car (cdr l))))))))))
	(t (simplify (cons (car sAnd) (cons (simplify (car l)) (simplify (cdr l))))))))

(defun andSimplify (l)
	(cond  
	((null l) l)
	((atom (car l))
		(cond
		((equal (car l) 1) (simplify (simplify (car (cdr l)))))
		((equal (car l) 0) 0)
		((equal (car l) nil) 0)
		(t (cond
			((atom (car (cdr l)))
				(cond
				((equal (car (cdr l)) 1) (simplify (car l)))
				((equal (car (cdr l)) 0) 0)
				((equal (car (cdr l)) nil) 0)
				((equal (car (cdr l)) (car l)) (simplify (car l)))
				(t (cons (car sAnd) (simplify l)))))
			(t (list (car sAnd) (simplify (car l)) (simplify (simplify (car (cdr l))))))))))
	(t (simplify (cons (car sAnd) (cons (simplify (car l)) (simplify (cdr l))))))))

(defun notSimplify (l)
	(cond
	((null l) l)
	((atom l)
		(cond
		((equal l 1) 0)
		((equal l 0) 1)
		((equal l nil) 1)
		(t l)))
	((atom (car l))
		(cond
		((equal (cdr l) nil)
			(cond
			((equal (car l) 1) 0)
			((equal (car l) 0) 1)
			((equal (car l) nil) 1)
			(t (list (car sNot) (car l)))))
		((equal (car l) 1)(cons nil (cdr l)))
		((equal (car l) 0)(cons 1 (cdr l)))
		((equal (car l) nil)(cons 1 (cdr l)))
		(t (list (car sNot) l))))
	((atom (simplify (cons (car sNot) (car (simplify l)))))
		(simplify (cons (car sNot) (car (simplify l)))))
	(t (cond
	   ((equal (car (car l)) (car sNot))
		(simplify (cons (car sNot) (car (simplify l)))))
	   ((equal (car (car l)) (car sAnd))
		(list (car sAnd) (list (car sNot) (car (cdr (car l)))) (list (car sNot) (car (cdr (cdr (car l)))))))
	   ((equal (car (car l)) (car sOr))
		(list (car sOr) (list (car sNot) (car (cdr (car l)))) (list (car sNot) (car (cdr (cdr (car l)))))))
	   (t (cons (car sNot) l))))))
