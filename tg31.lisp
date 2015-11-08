;;;; Projeto IA
;;;; 1 Sem 2015-2016
;;;; Grupo tg31


; replace with (load "utils.fas")
;(load (compile-file "utils.lisp"))




;;; 2.1 Tipos

;;; 2.1.1	Tipo Accao
(defstruct accao
	coluna
	peca)

;; cria-accao : inteiro x array -> accao
(defun cria-accao (c p)
	(make-accao :coluna c :peca p))


; definidos automaticamente:
;; accao-coluna : accao -> inteiro
;; accao-peca : accao -> array


;;; 2.1.2	Tipo Tabuleiro

;; cria-tabuleiro : void -> tabuleiro
(defun cria-tabuleiro ()
	(make-array '(18 10)))

;; copia-tabuleiro : tabuleiro -> tabuleiro
(defun copia-tabuleiro (tab)
	(let ((novo (cria-tabuleiro)))
		(dotimes (i 18)
		(dotimes (j 10)
			(setf (aref novo i j) (aref tab i j))))
		novo))

;; tabuleiro-preenchido-p : tabuliro x inteiro x inteiro -> logico
(defun tabuleiro-preenchido-p (tab l c)
	(aref tab (- 17 l) c))

;; tabuleiro-altura-coluna : tabuleiro x inteiro -> inteiro
(defun tabuleiro-altura-coluna (tab c)
	(dotimes (i 18) ; each line
		(let ((i (- 17 i))) ; quermos comecar em cima. 0 e' em baixo
			(when (tabuleiro-preenchido-p tab i c)
				(return-from tabuleiro-altura-coluna (+ i 1)))))
	0)

;; tabuleiro-linha-completa-p : tabuleiro x inteiro -> logico
(defun tabuleiro-linha-completa-p (tab l)
	(dotimes (j 10) ; each column
		(unless (tabuleiro-preenchido-p tab l j)
			(return-from tabuleiro-linha-completa-p nil)))
	t)

; delete this:
;	(let ((res))
;		(setf res (dotimes (j 10) ; each column
;			(unless (tabuleiro-preenchido-p tab l j)
;				(return nil))))
;		(if (null res) ; what is dis? shouldn't it return T?
;			0
;			res)))
; /\ nao tinha acabado

; isto nao esta' no enunciado :O \/
;; tabuleiro-linha-vazia-p : tabuleiro x inteiro -> logico
;(defun tabuleiro-linha-vazia-p (tab l)
;	(let ((res))
;		(setf res (dotimes (j 10) ; each column
;			(when (tabuleiro-preenchido-p tab l j)
;				(return nil))))
;		(if (null res)
;			0
;			res)))

;; tabuleiro-preenche! : tabuleiro x inteiro x inteiro -> {}
(defun tabuleiro-preenche! (tab l c)
	(if (and (and (>= l 0) (< l 18)) (and (>= c 0) (< c 10)))
		(setf (aref tab (- 17 l) c) T)))

;; tabuleiro-remove-linha! : tabuleiro x inteiro -> {}
(defun tabuleiro-remove-linha! (tab l)
	(let ((n (- 17 l))) ; numero de linhas para mover
		(dotimes (i n)
			(let ((ii (- n i))) ; de baixo pra cima
				(dotimes (j 10) ; copia a linha de cima para baixo
					(setf (aref tab ii j) (aref tab (- ii 1) j))))))
	; poe a linha de cima a nil
	(dotimes (j 10)
		(setf (aref tab 0 j) nil)))

;	(unless (tabuleiro-linha-vazia-p l)
;		(let ((j 0))
;			(dotimes (j 10) ; each column
;				(setf (aref tab l j) (aref tab (+ l 1) j)))) ; copia l+1 para l
;			(tabuleiro-remove-linha! (+ l 1)))) ; recursively ?


;;;; need to test from here down \/

;; tabuleiro-topo-preenchido : tabuleiro -> logico
(defun tabuleiro-topo-preenchido (tab)
	(dotimes (j 10) ; columns
		(when (aref tab 0 j)
			(return-from tabuleiro-topo-preenchido T)))
	nil)
; /\ mais eficiente
;	(let (j 0)
;		(dotimes (j 10)
;			(when (= (tabuleiro-altura-coluna j) 17)
;				(return-from tabuleiro-topo-preenchido T))))
;		(nil)) ; false if not found


;; tabuleiros-iguais : tabuleiro x tabuleiro -> logico
(defun tabuleiros-iguais (tab1 tab2)
	(dotimes (i 18)
	(dotimes (j 10)
		(unless (= (aref tab1 i j) (aref tab2 i j))
			(return-from tabuleiros-iguais nil))))
	T)

;; tabuleiro->array : tabuleiro -> array
(defun tabuleiro->array (tab)
	(let ((novo (cria-tabuleiro)))
		(dotimes (i 18)
		(dotimes (j 10)
			(setf (aref novo (- 17 i) j) (aref tab i j))))
		novo))


;;; 2.1.3	Tipo Estado

(defstruct estado
	(pontos 1337)
	pecas-por-colocar
	pecas-colocadas
	tabuleiro)


;; copia-estado : estado -> estado
(defun copia-estado (estado)
	(copy-structure estado))

;; estados-iguais-p : estado x estado -> logico
(defun estados-iguais-p (estado1 estado2)
	(equal estado1 estado2)) ; should be wrong

;; estado-final-p : estado ->logico
(defun estado-final-p (estado)
	(let (score :pontos)
		(print score))
	; returns if estado.pecas-por-colocar == nil || estado.tabuleiro.tabuleiro-topo-preenchido == T
)

;;; 2.1.4	Tipo Problema

(defstruct problema
	estado-inicial
	solucoes
	accoes
	resultado
	custo-caminho
)

;;; 2.2 Funcoes

;;; 2.2.1 Funcoes do problema


;; solucao : estado -> logico
(defun solucao (estado)
	nil ; returns if estado.pecas-por-colocar == nil && estado.tabuleiro.tabuleiro-topo-preenchido == nil
)


;; accoes : estado -> lista
(defun accoes (estado)
	nil ; returns estado.pecas-por-colocar[0] possible combinations
)

