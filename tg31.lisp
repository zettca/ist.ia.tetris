;;;; Projeto IA
;;;; 1 Sem 2015-2016
;;;; Grupo tg31


; replace with (load "utils.fas")
;(load (compile-file "utils.lisp"))



;;; Funcoes auxiliares




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

;; tabuleiro-preenche! : tabuleiro x inteiro x inteiro -> {}
(defun tabuleiro-preenche! (tab l c)
	(if (and (>= l 0) (< l 18) (>= c 0) (< c 10))
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


;; tabuleiro-topo-preenchido : tabuleiro -> logico
(defun tabuleiro-topo-preenchido (tab)
	(dotimes (j 10) ; columns
		(unless (aref tab 0 j)
			(return-from tabuleiro-topo-preenchido nil)))
	T)

;; not tested
;; tabuleiros-iguais : tabuleiro x tabuleiro -> logico
(defun tabuleiros-iguais (tab1 tab2)
	(dotimes (i 18)
	(dotimes (j 10)
		(unless (= (aref tab1 i j) (aref tab2 i j))
			(return-from tabuleiros-iguais nil))))
	T)

;; tabuleiro->array : tabuleiro -> array
(defun tabuleiro->array (tab)
	(let ((novo (make-array '(18 10))))
		(dotimes (i 18)
		(dotimes (j 10)
			(setf (aref novo (- 17 i) j) (aref tab i j))))
		novo))


;;; 2.1.3	Tipo Estado

(defstruct estado
	(pontos 0)
	pecas-por-colocar
	pecas-colocadas
	tabuleiro)


;; copia-estado : estado -> estado
(defun copia-estado (e)
	(let ((novo))
		(setf novo (make-estado))
		; temos de copiar o tabuleiro e as listas,
		; otherwise fica uma ref para a mesma
		(setf (estado-pontos novo) (estado-pontos e))
		(setf (estado-pecas-por-colocar novo)
					(copy-list (estado-pecas-por-colocar e)))
		(setf (estado-pecas-colocadas novo)
					(copy-list (estado-pecas-colocadas e)))
		(setf (estado-tabuleiro novo)
			  (copia-tabuleiro (estado-tabuleiro e)))
		novo))

;; estados-iguais-p : estado x estado -> logico
(defun estados-iguais-p (e1 e2)
	; temos de comparar os campos todos, em especial o tabuleiro
	(and (equal (estado-pontos e1) (estado-pontos e2))
		 (equal (estado-pecas-por-colocar e1) (estado-pecas-por-colocar e2))
		 (equal (estado-pecas-colocadas e1) (estado-pecas-colocadas e2))
		 (tabuleiros-iguais-p (estado-tabuleiro e1) (estado-tabuleiro e2))))

;; estado-final-p : estado ->logico
(defun estado-final-p (estado)
	(or (not (estado-pecas-por-colocar estado)) ; if empty
		(tabuleiro-topo-preenchido (estado-tabuleiro estado))))

;;; 2.1.4	Tipo Problema

(defstruct problema
	estado-inicial		; estado
	solucao				; funcao : estado -> logico
	accoes				; funcao : estado -> lista de accoes
	resultado			; funcao : estado x accao -> estado
	custo-caminho)		; funcao : estado -> inteiro

;;; 2.2 Funcoes

;;; 2.2.1 Funcoes do problema


;; solucao : estado -> logico
(defun solucao (estado)
	(and (not (estado-pecas-por-colocar estado))
		 (not (tabuleiro-topo-preenchido (estado-tabuleiro estado)))))

;; accoes : estado -> lista
(defun accoes (estado)
	nil)
	;
	;
	;

;; resultado : estado x accao -> estado
(defun resultado (estado accao)
	nil) ; aplica accao no estado atual. retorna novo estado

;; qualidade : estado -> inteiro
(defun qualidade (estado)
	nil) ; returns CUSTO do estado. should be negative

;; custo-oportunidade : estado -> inteiro
(defun custo-oportunidade (estado)
	nil) ;