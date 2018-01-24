;;
;; Time of Generation:
;; Wed Apr 12 00:35:16 EDT 2017
;;

;;
;;;;;;
;
; HX-2016-05:
; for Scheme code
; translated from ATS
;
;;;;;;
;;
;; For some implementations of scheme,
;; we may have to map [define-macro] to [defmacro]
;;
;; (defmacro (define-macro x y) `(defmacro ,x ,y))
;;
;;;;;;

;; ****** ****** ;;

(define atscc2scm_null '())

;; ****** ****** ;;

(define atscc2scm_true #t)
(define atscc2scm_false #f)

;; ****** ****** ;;

(define-macro
 (ats2scmpre_list_nil) atscc2scm_null)
(define-macro
 (ats2scmpre_list_cons x xs) `(cons ,x ,xs))

;; ****** ****** ;;
;;
(define ATSINSmove0_void atscc2scm_null)
;;
(define-macro (ATSINSmove1_void cmd) cmd)
;;
;; ****** ****** ;;
;;
(define-macro
 (ATSINStmpset tmp val) `(set! ,tmp ,val)
)
(define-macro
 (ATSINSstatmpset statmp val) `(set! ,statmp ,val)
)
;;
(define-macro
 (ATSdynloadset flag val) `(set! ,flag ,val)
)
(define-macro
 (ATSINSdyncst_valbind d2cst val) `(set! ,d2cst ,val)
)
;;
;; ****** ****** ;;

(define-macro (ATSfunclo_fun fc) fc)
(define-macro (ATSfunclo_fclo fc) `(car ,fc))

;; ****** ****** ;;

(define-macro (ATSCKiseqz x) `(= ,x 0))
(define-macro (ATSCKisneqz x) `(not (= ,x 0)))

;; ****** ****** ;;
;;
(define-macro (ATSCKpat_int x y) `(= ,x ,y))
;;
(define-macro (ATSCKpat_bool x y) `(eqv? ,x ,y))
;;
(define-macro (ATSCKpat_string x y) `(eqv? ,x ,y))
;;
;; ****** ****** ;;
;;
(define-macro (ATSCKpat_con0 x tag) `(= ,x ,tag))
(define-macro (ATSCKpat_con1 x tag) `(= (car ,x) ,tag))
;;
;; ****** ****** ;;

(define-macro (ATSCKptrisnull x) `(eqv? ,x atscc2scm_null))
(define-macro (ATSCKptriscons x) `(not (eqv? ,x atscc2scm_null)))

;; ****** ****** ;;
;;
(define-macro (ATSCCget_0 xs) `(car ,xs))
(define-macro (ATSCCget_1 xs) `(car (cdr ,xs)))
(define-macro (ATSCCget_2 xs) `(car (cdr (cdr ,xs))))
(define-macro (ATSCCget_3 xs) `(car (cdr (cdr (cdr ,xs)))))
;;
(define-macro (ATSCCget_at xs n) `(list-ref ,xs ,n))
;;
(define-macro (ATSCCset_0 xs x0) `(set-car! ,xs ,x0))
(define-macro (ATSCCset_1 xs x0) `(set-car! (cdr ,xs) ,x0))
(define-macro (ATSCCset_2 xs x0) `(set-car! (cdr (cdr ,xs)) ,x0))
(define-macro (ATSCCset_3 xs x0) `(set-car! (cdr (cdr (cdr ,xs))) ,x0))
;;
;; ****** ****** ;;
;;
(define-syntax
 ATSPMVtyrec (syntax-rules () ((_ . xs) (list . xs))))
(define-syntax
 ATSPMVtysum (syntax-rules () ((_ . xs) (list . xs))))
;;
;; ****** ****** ;;
;;
(define-macro
 (ATSPMVlazyval fc) `(list 0 ,fc))
;;
(define
 (ATSPMVlazyval_eval lazyval)
 (let ((flag (ATSCCget_0 lazyval)))
   (if (= flag 0)
     (begin
      (ATSCCset_0 lazyval 1)
      (let ((thunk (ATSCCget_1 lazyval)))
	(let ((result ((ATSfunclo_fclo thunk) thunk)))
          (ATSCCset_1 lazyval result) result))
     )
     (begin
      (ATSCCset_0 lazyval (+ flag 1)) (ATSCCget_1 lazyval)
     )
   ) ;; if
 ) ;; let
) ;; define
;;
;; ****** ****** ;;
;;
(define-macro
 (ATSPMVllazyval thunk) thunk)
;;
(define
 (ATSPMVllazyval_eval llazyval)
  ((ATSfunclo_fclo llazyval) llazyval #t)
) ;; define
;;
(define
 (atspre_lazy_vt_free llazyval)
  ((ATSfunclo_fclo llazyval) llazyval #f)
) ;; define
;;
;; ****** ****** ;;

(define-macro (ATSSELcon xs i) `(ATSCCget_at ,xs ,i))
(define-macro (ATSSELboxrec xs i) `(ATSCCget_at ,xs ,i))

;; ****** ****** ;;
;;
(define (ATSINSdeadcode_fail) (exit 1))
;;
(define
 (ATSINScaseof_fail msg)
 (begin
  (display msg (current-error-port))
  (exit 1)
 )
) ;; end-of-define
;;
;; ****** ****** ;;
;;
(define
 (ats2scmpre_assert_bool0 tfv) (if (not tfv) (exit 1)))
(define
 (ats2scmpre_assert_bool1 tfv) (if (not tfv) (exit 1)))
;;
(define
 (ats2scmpre_assert_errmsg_bool0 msg tfv)
 (if (not tfv)
   (begin (display msg (current-error-port)) (exit 1))
 )
) ;; end-of-define
(define
 (ats2scmpre_assert_errmsg_bool1 msg tfv)
 (if (not tfv)
   (begin (display msg (current-error-port)) (exit 1))
 )
) ;; end-of-define
;;
;; ****** ****** ;;

(define-macro
 (ats2scmpre_cloref0_app cf)
`(let ((cf1 ,cf)) ((ATSfunclo_fclo cf1) cf1))
) ; define-macro
(define-macro
 (ats2scmpre_cloref1_app cf x)
`(let ((cf1 ,cf)) ((ATSfunclo_fclo cf1) cf1 ,x))
) ; define-macro
(define-macro
 (ats2scmpre_cloref2_app cf x1 x2)
`(let ((cf1 ,cf)) ((ATSfunclo_fclo cf1) cf1 x1 x2))
) ; define-macro
(define-macro
 (ats2scmpre_cloref3_app cf x1 x2 x3)
`(let ((cf1 ,cf)) ((ATSfunclo_fclo cf1) cf1 ,x1 ,x2 ,x3))
) ; define-macro

;; ****** ****** ;;

(define
 (ats2scmpre_cloref2fun0 cf)
 (lambda () (ats2scmpre_cloref0_app cf))
) ; define
(define
 (ats2scmpre_cloref2fun1 cf)
 (lambda (x) (ats2scmpre_cloref1_app cf x))
) ; define
(define
 (ats2scmpre_cloref2fun2 cf)
 (lambda (x1 x2) (ats2scmpre_cloref2_app cf x1 x2))
) ; define
(define
 (ats2scmpre_cloref2fun3 cf)
 (lambda (x1 x2 x3) (ats2scmpre_cloref3_app cf x1 x2 x3))
) ; define

;; ****** ****** ;;

(define-macro
 (ats2scmpre_lazy2cloref lazyval) `(ATSCCget_1 ,lazyval)
) ;; end-of-define

;; ****** ****** ;;

;;;;;; end of [basics_cats.scm] ;;;;;;
;;
;;;;;;
;
; HX-2016-05:
; for Scheme code
; translated from ATS
;
;;;;;;
;;

;;
;;;;;;
; beg of [integer_cats.scm]
;;;;;;
;;

;; ****** ****** ;;
;;
;; HX: for signed integers
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_neg_int0 x) `(- ,x)
)
(define-macro
 (ats2scmpre_neg_int1 x) `(- ,x)
)
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_abs_int0 x) `(abs ,x)
)
(define-macro
 (ats2scmpre_abs_int1 x) `(abs ,x)
)
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_succ_int0 x) `(+ ,x 1)
)
(define-macro
 (ats2scmpre_pred_int0 x) `(- ,x 1)
)
;;
(define-macro
 (ats2scmpre_succ_int1 x) `(+ ,x 1)
)
(define-macro
 (ats2scmpre_pred_int1 x) `(- ,x 1)
)
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_add_int0_int0 x y) `(+ ,x ,y)
)
(define-macro
 (ats2scmpre_sub_int0_int0 x y) `(- ,x ,y)
)
(define-macro
 (ats2scmpre_mul_int0_int0 x y) `(* ,x ,y)
)
(define-macro
 (ats2scmpre_div_int0_int0 x y) `(quotient ,x ,y)
)
;;
(define-macro
 (ats2scmpre_mod_int0_int0 x y) `(modulo ,x ,y)
)
(define-macro
 (ats2scmpre_rem_int0_int0 x y) `(remainder ,x ,y)
)
;;
(define-macro
 (ats2scmpre_add_int1_int1 x y) `(+ ,x ,y)
)
(define-macro
 (ats2scmpre_sub_int1_int1 x y) `(- ,x ,y)
)
(define-macro
 (ats2scmpre_mul_int1_int1 x y) `(* ,x ,y)
)
(define-macro
 (ats2scmpre_div_int1_int1 x y) `(quotient ,x ,y)
)
;;
(define-macro
 (ats2scmpre_mod_int1_int1 x y) `(modulo ,x ,y)
)
(define-macro
 (ats2scmpre_nmod_int1_int1 x y) `(modulo ,x ,y)
)
;;
(define-macro
 (ats2scmpre_rem_int1_int1 x y) `(remainder ,x ,y)
)
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_lt_int0_int0 x y) `(< ,x ,y)
)
(define-macro
 (ats2scmpre_lte_int0_int0 x y) `(<= ,x ,y)
)
(define-macro
 (ats2scmpre_gt_int0_int0 x y) `(> ,x ,y)
)
(define-macro
 (ats2scmpre_gte_int0_int0 x y) `(>= ,x ,y)
)
;;
(define-macro
 (ats2scmpre_eq_int0_int0 x y) `(= ,x ,y)
)
(define-macro
 (ats2scmpre_neq_int0_int0 x y) `(not (= ,x ,y))
)
;;
(define-macro
 (ats2scmpre_lt_int1_int1 x y) `(< ,x ,y)
)
(define-macro
 (ats2scmpre_lte_int1_int1 x y) `(<= ,x ,y)
)
(define-macro
 (ats2scmpre_gt_int1_int1 x y) `(> ,x ,y)
)
(define-macro
 (ats2scmpre_gte_int1_int1 x y) `(>= ,x ,y)
)
;;
(define-macro
 (ats2scmpre_eq_int1_int1 x y) `(= ,x ,y)
)
(define-macro
 (ats2scmpre_neq_int1_int1 x y) `(not (= ,x ,y))
)
;;
(define-macro
 (ats2scmpre_compare_int0_int0 x y)
`(let ((x1 ,x) (y1 ,y)) (if (< x1 y1) -1 (if (<= x1 y1) 0 1)))
)
(define-macro
 (ats2scmpre_compare_int1_int1 x y)
`(let ((x1 ,x) (y1 ,y)) (if (< x1 y1) -1 (if (<= x1 y1) 0 1)))
)
;;
;; ****** ****** ;;

;; end of [integer_cats.scm] ;;
;;
;;;;;;
;
; HX-2016-06:
; for Scheme code
; translated from ATS
;
;;;;;;
;;

;;
;;;;;;
; beg of [bool_cats.scm]
;;;;;;
;;

;; ****** ****** ;;

(define-macro
 (ats2scmpre_neg_bool0 x) `(not ,x)
)
(define-macro
 (ats2scmpre_neg_bool1 x) `(not ,x)
)

;; ****** ****** ;;

;; end of [bool_cats.scm] ;;
;;
;;;;;;
;
; HX-2016-05:
; for Scheme code
; translated from ATS
;
;;;;;;
;;

;;
;;;;;;
; beg of [float_cats.scm]
;;;;;;
;;

;; ****** ****** ;;
;;
;; HX: for signed floats
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_int2double x) x)
(define-macro
 (ats2scmpre_double_of_int x) x)
;;
(define-macro
 (ats2scmpre_double2int x) `(truncate ,x))
(define-macro
 (ats2scmpre_int_of_double x) `(truncate ,x))
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_neg_double x) `(- ,x)
)
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_abs_double x) `(abs ,x)
)
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_succ_double x) `(+ ,x 1)
)
(define-macro
 (ats2scmpre_pred_double x) `(- ,x 1)
)
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_add_int_double x y) `(+ ,x ,y)
)
(define-macro
 (ats2scmpre_sub_int_double x y) `(- ,x ,y)
)
(define-macro
 (ats2scmpre_mul_int_double x y) `(* ,x ,y)
)
(define-macro
 (ats2scmpre_div_int_double x y) `(/ ,x ,y)
)
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_add_double_int x y) `(+ ,x ,y)
)
(define-macro
 (ats2scmpre_sub_double_int x y) `(- ,x ,y)
)
(define-macro
 (ats2scmpre_mul_double_int x y) `(* ,x ,y)
)
(define-macro
 (ats2scmpre_div_double_int x y) `(/ ,x ,y)
)
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_add_double_double x y) `(+ ,x ,y)
)
(define-macro
 (ats2scmpre_sub_double_double x y) `(- ,x ,y)
)
(define-macro
 (ats2scmpre_mul_double_double x y) `(* ,x ,y)
)
(define-macro
 (ats2scmpre_div_double_double x y) `(/ ,x ,y)
)
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_lt_double_double x y) `(< ,x ,y)
)
(define-macro
 (ats2scmpre_lte_double_double x y) `(<= ,x ,y)
)
(define-macro
 (ats2scmpre_gt_double_double x y) `(> ,x ,y)
)
(define-macro
 (ats2scmpre_gte_double_double x y) `(>= ,x ,y)
)
;;
(define-macro
 (ats2scmpre_eq_double_double x y) `(= ,x ,y)
)
(define-macro
 (ats2scmpre_neq_double_double x y) `(not (= ,x ,y))
)
;;
;; ****** ****** ;;

;; end of [float_cats.scm] ;;
;;
;;;;;;
;
; HX-2016-06:
; for Scheme code
; translated from ATS
;
;;;;;;
;;

;;
;;;;;;
; beg of [print_cats.scm]
;;;;;;
;;

;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_print_int x) `(display ,x))
;;
(define-macro
 (ats2scmpre_print_bool x)
`(display (if ,x "true" "false")))
(define-macro
 (ats2scmpre_print_char x) `(display ,x))
;;
(define-macro
 (ats2scmpre_print_double x) `(display ,x))
(define-macro
 (ats2scmpre_print_string x) `(display ,x))
;;
(define-macro
 (ats2scmpre_print_SCMval x) `(display ,x))
;;
;; ****** ****** ;;
;;
(define-macro (ats2scmpre_print_newline) `(newline))
;;
;; ****** ****** ;;

(define-macro
 (ats2scmpre_fprint_int out x) `(display ,x ,out))
;;
(define-macro
 (ats2scmpre_fprint_bool out x)
`(display (if ,x "true" "false") ,out))
(define-macro
 (ats2scmpre_fprint_char out x) `(display ,x ,out))
;;
(define-macro
 (ats2scmpre_fprint_double out x) `(display ,x ,out))
(define-macro
 (ats2scmpre_fprint_string out x) `(display ,x ,out))
(define-macro
 (ats2scmpre_fprint_SCMval out x) `(display ,x ,out))
;;
;; ****** ****** ;;
;;
(define-macro (ats2scmpre_fprint_newline out) `(newline ,out))
;;
;; ****** ****** ;;

;; end of [print_cats.scm] ;;
;;
;;;;;;
;
; HX-2016-05:
; for Scheme code
; translated from ATS
;
;;;;;;
;;

;;
;;;;;;
; beg of [reference_cats.scm]
;;;;;;
;;

;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_ref x) `(list ,x))
(define-macro
 (ats2scmpre_ref_make_elt x) `(list ,x))
;;
;; ****** ****** ;;
;;
(define-macro
 (ats2scmpre_ref_get_elt r) `(car ,r))
(define-macro
 (ats2scmpre_ref_set_elt r x0) `(set-car! ,r ,x0))
;;
(define-syntax
 ats2scmpre_ref_exch_elt
 (syntax-rules ()
  ((_ r x0) (let ((tmp (car r))) (set-car! r x0) tmp))
 )
)
;;
;; ****** ****** ;;

;; end of [reference_cats.scm] ;;
;;;;;;
;
; HX-2016-06:
; for Scheme code translated from ATS
;
;;;;;;

;;;;;;
;beg of [filebas_cats.scm]
;;;;;;
;;
(define-macro
 (ats2scmpre_stdin_get) `(current=input-port))
(define-macro
 (ats2scmpre_stdout_get) `(current-output-port))
(define-macro
 (ats2scmpre_stderr_get) `(current-error-port))
;;
;;;;;;
;;
(define-macro
 (ats2scmpre_fileref_close_input inp) `(close-input-port ,(inp))
) ;; define-macro
;;
(define-macro
 (ats2scmpre_fileref_open_input_exn fname) `(open-input-file ,(fname))
) ;; define-macro
;;
;;;;;;
;;
(define-macro
 (ats2scmpre_write_char c) `(write-char ,(c))) 
(define-macro
 (ats2scmpre_fwrite_char out c) `(write-char ,(c) ,(out))) 
;;
(define-macro
 (ats2scmpre_write_scmval scmval) `(write ,(scmval))) 
(define-macro
 (ats2scmpre_fwrite_scmval out scmval) `(write ,(scmval) ,(out))) 
;;
(define-macro
 (ats2scmpre_fileref_close_output inp) `(close-output-port ,(inp))
) ;; define-macro
;;
(define-macro
 (ats2scmpre_fileref_open_output_exn fname) `(open-output-file ,(fname))
) ;; define-macro
;;
;;;;;;
;;;;;; end of [filebas_cats.scm] ;;;;;;
;;;;;;
;
; HX-2016-06:
; for Scheme code translated from ATS
;
;;;;;;

;;;;;;
; beg of [SCMlist_cats.scm]
;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(define-macro
 (ats2scmpre_SCMlist_nil) `(list))
(define-macro
 (ats2scmpre_SCMlist_sing x) `(list ,x))
(define-macro
 (ats2scmpre_SCMlist_pair x1 x2) `(list ,x1 ,x2))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro
 (ats2scmpre_SCMlist_cons x0 xs) `(cons ,x0 ,xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (define
;;  (ats2scmpre_SCMlist_make_elt n x0)
;;  (letrec
;;    ((loop
;;      (lambda(n res)
;;       (if (> n 0) (loop (- n 1) (ats2scmpre_SCMlist_cons x0 res)) res))
;;     )
;;    ) (loop n (ats2scmpre_SCMlist_nil))
;;  ) ;; letrec
;; ) ;; define-ats2scmpre_SCMlist_make_elt
(define-macro
 (ats2scmpre_SCMlist_make_elt n x0) `(make-list ,n ,x0))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro
 (ats2scmpre_SCMlist_is_nil xs) `(null? ,xs))
(define-macro
 (ats2scmpre_SCMlist_is_cons xs) `(not (null? ,xs)))
(define-macro
 (ats2scmpre_SCMlist_isnot_nil xs) `(not (null? ,xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(define-macro
 (ats2scmpre_SCMlist_length xs) `(length ,xs))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro (ats2scmpre_SCMlist_head xs) `(car ,xs))
(define-macro (ats2scmpre_SCMlist_tail xs) `(cdr ,xs))
(define-macro (ats2scmpre_SCMlist_last_pair xs) `(last-pair ,xs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(define-macro
 (ats2scmpre_SCMlist_get_at xs i) `(list-ref ,xs ,i))
(define-macro
 (ats2scmpre_SCMlist_set_at xs i x0) `(list-set! ,xs ,i ,x0))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(define-macro
 (ats2scmpre_SCMlist_append xs ys) `(append ,xs ,ys))
;;
(define-macro (ats2scmpre_SCMlist_reverse xs) `(reverse ,xs))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Note that [fold] is is SRFI-1
;; So this one is implemented in list.dats
;; 
;;(define-macro
;; (ats2scmpre_SCMlist2list_rev xs)
;;`(fold (lambda (x xs) (cons x xs)) '() ,xs)
;;) ; define-macro
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-macro
 (ats2scmpre_SCMlist_sort_2 xs cmp)
`(let ((cmp (ats2scmpre_cloref2fun2 ,cmp)))
    (sort ,xs (lambda(x1 x2) (< (cmp x1 x2) 0))))
) ; define-macro

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; end of [SCMlist_cats.scm] ;;;;;;
;;;;;;
;;
;; The Scheme code is generated by atscc2scm
;; The starting compilation time is: 2017-4-12:  0h:35m
;;
;;;;;;

(define
(_ats2scmpre_list_patsfun_35__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_35 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_39__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_39 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_42__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_42 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_46__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_46 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_50__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_50 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_54__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_54 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_57__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_57 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_61__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_61 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_65__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_65 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_69__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_69 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_73__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_73 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_77__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_77 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_81__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_81 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_86__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_86 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_89__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_89 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_92__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_92 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_94__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_list_patsfun_94 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_list_patsfun_102__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0 xarg1)(_ats2scmpre_list_patsfun_102 (ATSCCget_1 _fcenvs_) xarg0 xarg1)) xenv0)
;;%}
) ;; define


;;fun
(define
(ats2scmpre_list_make_elt arg0 arg1)
(let(
;;knd = 0
  (tmpret2 #f)
  (tmp7 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_make_elt
  (ATSINStmpset tmp7 atscc2scm_null)
  (_ats2scmpre_list_loop_3 arg1 arg0 tmp7)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop_3 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret3 #f)
  (tmp4 #f)
  (tmp5 #f)
  (tmp6 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop_3
    (ATSINStmpset tmp4 (ats2scmpre_gt_int1_int1 arg0 0))
    (if tmp4
      (begin
       (ATSINStmpset tmp5 (ats2scmpre_sub_int1_int1 arg0 1))
       (ATSINStmpset tmp6 (ATSPMVtysum env0 arg1))
       ;; apy0 = tmp5
       ;; apy1 = tmp6
       ;; arg0 = apy0
       ;; arg1 = apy1
       ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop_3
       (_ats2scmpre_list_loop_3 env0 tmp5 tmp6)
      ) ;; if-then
      (begin
       arg1
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret3;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_make_intrange_2 arg0 arg1)
(let(
;;knd = 0
  (tmpret8 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_make_intrange_2
  (ats2scmpre_list_make_intrange_3 arg0 arg1 1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_make_intrange_3 arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret9 #f)
  (tmp20 #f)
  (tmp21 #f)
  (tmp22 #f)
  (tmp23 #f)
  (tmp24 #f)
  (tmp25 #f)
  (tmp26 #f)
  (tmp27 #f)
  (tmp28 #f)
  (tmp29 #f)
  (tmp30 #f)
  (tmp31 #f)
  (tmp32 #f)
  (tmp33 #f)
  (tmp34 #f)
  (tmp35 #f)
  (tmp36 #f)
  (tmp37 #f)
  (tmp38 #f)
  (tmp39 #f)
  (tmp40 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_make_intrange_3
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (ATSINStmpset tmp20 (ats2scmpre_gt_int0_int0 arg2 0))
       (if (not (ATSCKpat_bool tmp20 atscc2scm_true))
         (casefnx 2)
         (begin
          (ATSINStmpset tmp21 (ats2scmpre_lt_int0_int0 arg0 arg1))
          (if tmp21
            (begin
             (ATSINStmpset tmp25 (ats2scmpre_sub_int0_int0 arg1 arg0))
             (ATSINStmpset tmp24 (ats2scmpre_add_int0_int0 tmp25 arg2))
             (ATSINStmpset tmp23 (ats2scmpre_sub_int0_int0 tmp24 1))
             (ATSINStmpset tmp22 (ats2scmpre_div_int0_int0 tmp23 arg2))
             (ATSINStmpset tmp28 (ats2scmpre_sub_int0_int0 tmp22 1))
             (ATSINStmpset tmp27 (ats2scmpre_mul_int0_int0 tmp28 arg2))
             (ATSINStmpset tmp26 (ats2scmpre_add_int0_int0 arg0 tmp27))
             (ATSINStmpset tmp29 atscc2scm_null)
             (_ats2scmpre_list_loop1_6 tmp22 tmp26 arg2 tmp29)
            ) ;; if-then
            (begin
             atscc2scm_null
            ) ;; if-else
          )
         )
       )
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((2)
       (ATSINStmpset tmp30 (ats2scmpre_lt_int0_int0 arg2 0))
       (if (not (ATSCKpat_bool tmp30 atscc2scm_true))
         (casefnx 3)
         (begin
          (ATSINStmpset tmp31 (ats2scmpre_gt_int0_int0 arg0 arg1))
          (if tmp31
            (begin
             (ATSINStmpset tmp32 (ats2scmpre_neg_int0 arg2))
             (ATSINStmpset tmp36 (ats2scmpre_sub_int0_int0 arg0 arg1))
             (ATSINStmpset tmp35 (ats2scmpre_add_int0_int0 tmp36 tmp32))
             (ATSINStmpset tmp34 (ats2scmpre_sub_int0_int0 tmp35 1))
             (ATSINStmpset tmp33 (ats2scmpre_div_int0_int0 tmp34 tmp32))
             (ATSINStmpset tmp39 (ats2scmpre_sub_int0_int0 tmp33 1))
             (ATSINStmpset tmp38 (ats2scmpre_mul_int0_int0 tmp39 tmp32))
             (ATSINStmpset tmp37 (ats2scmpre_sub_int0_int0 arg0 tmp38))
             (ATSINStmpset tmp40 atscc2scm_null)
             (_ats2scmpre_list_loop2_7 tmp33 tmp37 tmp32 tmp40)
            ) ;; if-then
            (begin
             atscc2scm_null
            ) ;; if-else
          )
         )
       )
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop1_6 arg0 arg1 arg2 arg3)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
;;(apy3 #f)
  (tmpret10 #f)
  (tmp11 #f)
  (tmp12 #f)
  (tmp13 #f)
  (tmp14 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop1_6
    (ATSINStmpset tmp11 (ats2scmpre_gt_int0_int0 arg0 0))
    (if tmp11
      (begin
       (ATSINStmpset tmp12 (ats2scmpre_sub_int0_int0 arg0 1))
       (ATSINStmpset tmp13 (ats2scmpre_sub_int0_int0 arg1 arg2))
       (ATSINStmpset tmp14 (ATSPMVtysum arg1 arg3))
       ;; apy0 = tmp12
       ;; apy1 = tmp13
       ;; apy2 = arg2
       ;; apy3 = tmp14
       ;; arg0 = apy0
       ;; arg1 = apy1
       ;; arg2 = apy2
       ;; arg3 = apy3
       ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop1_6
       (_ats2scmpre_list_loop1_6 tmp12 tmp13 arg2 tmp14)
      ) ;; if-then
      (begin
       arg3
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret10;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop2_7 arg0 arg1 arg2 arg3)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
;;(apy3 #f)
  (tmpret15 #f)
  (tmp16 #f)
  (tmp17 #f)
  (tmp18 #f)
  (tmp19 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop2_7
    (ATSINStmpset tmp16 (ats2scmpre_gt_int0_int0 arg0 0))
    (if tmp16
      (begin
       (ATSINStmpset tmp17 (ats2scmpre_sub_int0_int0 arg0 1))
       (ATSINStmpset tmp18 (ats2scmpre_add_int0_int0 arg1 arg2))
       (ATSINStmpset tmp19 (ATSPMVtysum arg1 arg3))
       ;; apy0 = tmp17
       ;; apy1 = tmp18
       ;; apy2 = arg2
       ;; apy3 = tmp19
       ;; arg0 = apy0
       ;; arg1 = apy1
       ;; arg2 = apy2
       ;; arg3 = apy3
       ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop2_7
       (_ats2scmpre_list_loop2_7 tmp17 tmp18 arg2 tmp19)
      ) ;; if-then
      (begin
       arg3
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret15;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_length arg0)
(let(
;;knd = 0
  (tmpret52 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_length
  (_ats2scmpre_list_loop_14 arg0 0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop_14 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret53 #f)
  (tmp55 #f)
  (tmp56 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop_14
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg0)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg1
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp55 (ATSSELcon arg0 1))
         (ATSINStmpset tmp56 (ats2scmpre_add_int1_int1 arg1 1))
         ;; apy0 = tmp55
         ;; apy1 = tmp56
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop_14
         (_ats2scmpre_list_loop_14 tmp55 tmp56)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret53;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_last arg0)
(let(
;;knd = 1
;;(apy0 #f)
  (tmpret57 #f)
  (tmp58 #f)
  (tmp59 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_list_last
    (ATSINStmpset tmp58 (ATSSELcon arg0 0))
    (ATSINStmpset tmp59 (ATSSELcon arg0 1))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp59)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         tmp58
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         ;; apy0 = tmp59
         ;; arg0 = apy0
         ;; funlab_scm = 1; // __patsflab_list_last
         (ats2scmpre_list_last tmp59)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret57;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_get_at arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret60 #f)
  (tmp61 #f)
  (tmp62 #f)
  (tmp63 #f)
  (tmp64 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_list_get_at
    (ATSINStmpset tmp61 (ats2scmpre_eq_int1_int1 arg1 0))
    (if tmp61
      (begin
       (ATSINStmpset tmp62 (ATSSELcon arg0 0))
       tmp62
      ) ;; if-then
      (begin
       (ATSINStmpset tmp63 (ATSSELcon arg0 1))
       (ATSINStmpset tmp64 (ats2scmpre_sub_int1_int1 arg1 1))
       ;; apy0 = tmp63
       ;; apy1 = tmp64
       ;; arg0 = apy0
       ;; arg1 = apy1
       ;; funlab_scm = 1; // __patsflab_list_get_at
       (ats2scmpre_list_get_at tmp63 tmp64)
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret60;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_snoc arg0 arg1)
(let(
;;knd = 0
  (tmpret65 #f)
  (tmp66 #f)
  (tmp67 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_snoc
  (ATSINStmpset tmp67 atscc2scm_null)
  (ATSINStmpset tmp66 (ATSPMVtysum arg1 tmp67))
  (ats2scmpre_list_append arg0 tmp66)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_extend arg0 arg1)
(let(
;;knd = 0
  (tmpret68 #f)
  (tmp69 #f)
  (tmp70 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_extend
  (ATSINStmpset tmp70 atscc2scm_null)
  (ATSINStmpset tmp69 (ATSPMVtysum arg1 tmp70))
  (ats2scmpre_list_append arg0 tmp69)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_append arg0 arg1)
(let(
;;knd = 0
  (tmpret71 #f)
  (tmp72 #f)
  (tmp73 #f)
  (tmp74 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_append
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       arg1
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp72 (ATSSELcon arg0 0))
       (ATSINStmpset tmp73 (ATSSELcon arg0 1))
       (ATSINStmpset tmp74 (ats2scmpre_list_append tmp73 arg1))
       (ATSPMVtysum tmp72 tmp74)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_mul_int_list arg0 arg1)
(let(
;;knd = 0
  (tmpret75 #f)
  (tmp80 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_mul_int_list
  (ATSINStmpset tmp80 atscc2scm_null)
  (_ats2scmpre_list_loop_21 arg1 arg0 tmp80)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop_21 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret76 #f)
  (tmp77 #f)
  (tmp78 #f)
  (tmp79 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop_21
    (ATSINStmpset tmp77 (ats2scmpre_gt_int1_int1 arg0 0))
    (if tmp77
      (begin
       (ATSINStmpset tmp78 (ats2scmpre_sub_int1_int1 arg0 1))
       (ATSINStmpset tmp79 (ats2scmpre_list_append env0 arg1))
       ;; apy0 = tmp78
       ;; apy1 = tmp79
       ;; arg0 = apy0
       ;; arg1 = apy1
       ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop_21
       (_ats2scmpre_list_loop_21 env0 tmp78 tmp79)
      ) ;; if-then
      (begin
       arg1
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret76;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_reverse arg0)
(let(
;;knd = 0
  (tmpret81 #f)
  (tmp82 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_reverse
  (ATSINStmpset tmp82 atscc2scm_null)
  (ats2scmpre_list_reverse_append arg0 tmp82)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_reverse_append arg0 arg1)
(let(
;;knd = 0
  (tmpret83 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_reverse_append
  (_ats2scmpre_list_loop_24 arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop_24 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret84 #f)
  (tmp85 #f)
  (tmp86 #f)
  (tmp87 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop_24
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg0)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg1
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp85 (ATSSELcon arg0 0))
         (ATSINStmpset tmp86 (ATSSELcon arg0 1))
         (ATSINStmpset tmp87 (ATSPMVtysum tmp85 arg1))
         ;; apy0 = tmp86
         ;; apy1 = tmp87
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop_24
         (_ats2scmpre_list_loop_24 tmp86 tmp87)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret84;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_concat arg0)
(let(
;;knd = 0
  (tmpret88 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_concat
  (_ats2scmpre_list_auxlst_26 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_auxlst_26 arg0)
(let(
;;knd = 0
  (tmpret89 #f)
  (tmp90 #f)
  (tmp91 #f)
  (tmp92 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_auxlst_26
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp90 (ATSSELcon arg0 0))
       (ATSINStmpset tmp91 (ATSSELcon arg0 1))
       (ATSINStmpset tmp92 (_ats2scmpre_list_auxlst_26 tmp91))
       (ats2scmpre_list_append tmp90 tmp92)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_take arg0 arg1)
(let(
;;knd = 0
  (tmpret93 #f)
  (tmp94 #f)
  (tmp95 #f)
  (tmp96 #f)
  (tmp97 #f)
  (tmp98 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_take
  (ATSINStmpset tmp94 (ats2scmpre_gt_int1_int1 arg1 0))
  (if tmp94
    (begin
     (ATSINStmpset tmp95 (ATSSELcon arg0 0))
     (ATSINStmpset tmp96 (ATSSELcon arg0 1))
     (ATSINStmpset tmp98 (ats2scmpre_sub_int1_int1 arg1 1))
     (ATSINStmpset tmp97 (ats2scmpre_list_take tmp96 tmp98))
     (ATSPMVtysum tmp95 tmp97)
    ) ;; if-then
    (begin
     atscc2scm_null
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_drop arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret99 #f)
  (tmp100 #f)
  (tmp101 #f)
  (tmp102 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_list_drop
    (ATSINStmpset tmp100 (ats2scmpre_gt_int1_int1 arg1 0))
    (if tmp100
      (begin
       (ATSINStmpset tmp101 (ATSSELcon arg0 1))
       (ATSINStmpset tmp102 (ats2scmpre_sub_int1_int1 arg1 1))
       ;; apy0 = tmp101
       ;; apy1 = tmp102
       ;; arg0 = apy0
       ;; arg1 = apy1
       ;; funlab_scm = 1; // __patsflab_list_drop
       (ats2scmpre_list_drop tmp101 tmp102)
      ) ;; if-then
      (begin
       arg0
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret99;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_split_at arg0 arg1)
(let(
;;knd = 0
  (tmpret103 #f)
  (tmp104 #f)
  (tmp105 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_split_at
  (ATSINStmpset tmp104 (ats2scmpre_list_take arg0 arg1))
  (ATSINStmpset tmp105 (ats2scmpre_list_drop arg0 arg1))
  (ATSPMVtyrec tmp104 tmp105)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_insert_at arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret106 #f)
  (tmp107 #f)
  (tmp108 #f)
  (tmp109 #f)
  (tmp110 #f)
  (tmp111 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_insert_at
  (ATSINStmpset tmp107 (ats2scmpre_gt_int1_int1 arg1 0))
  (if tmp107
    (begin
     (ATSINStmpset tmp108 (ATSSELcon arg0 0))
     (ATSINStmpset tmp109 (ATSSELcon arg0 1))
     (ATSINStmpset tmp111 (ats2scmpre_sub_int1_int1 arg1 1))
     (ATSINStmpset tmp110 (ats2scmpre_list_insert_at tmp109 tmp111 arg2))
     (ATSPMVtysum tmp108 tmp110)
    ) ;; if-then
    (begin
     (ATSPMVtysum arg2 arg0)
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_remove_at arg0 arg1)
(let(
;;knd = 0
  (tmpret112 #f)
  (tmp113 #f)
  (tmp114 #f)
  (tmp115 #f)
  (tmp116 #f)
  (tmp117 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_remove_at
  (ATSINStmpset tmp113 (ATSSELcon arg0 0))
  (ATSINStmpset tmp114 (ATSSELcon arg0 1))
  (ATSINStmpset tmp115 (ats2scmpre_gt_int1_int1 arg1 0))
  (if tmp115
    (begin
     (ATSINStmpset tmp117 (ats2scmpre_sub_int1_int1 arg1 1))
     (ATSINStmpset tmp116 (ats2scmpre_list_remove_at tmp114 tmp117))
     (ATSPMVtysum tmp113 tmp116)
    ) ;; if-then
    (begin
     tmp114
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_takeout_at arg0 arg1)
(let(
;;knd = 0
  (tmpret118 #f)
  (tmp119 #f)
  (tmp120 #f)
  (tmp121 #f)
  (tmp122 #f)
  (tmp123 #f)
  (tmp124 #f)
  (tmp125 #f)
  (tmp126 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_takeout_at
  (ATSINStmpset tmp119 (ATSSELcon arg0 0))
  (ATSINStmpset tmp120 (ATSSELcon arg0 1))
  (ATSINStmpset tmp121 (ats2scmpre_gt_int1_int1 arg1 0))
  (if tmp121
    (begin
     (ATSINStmpset tmp123 (ats2scmpre_sub_int1_int1 arg1 1))
     (ATSINStmpset tmp122 (ats2scmpre_list_takeout_at tmp120 tmp123))
     (ATSINStmpset tmp124 (ATSSELboxrec tmp122 0))
     (ATSINStmpset tmp125 (ATSSELboxrec tmp122 1))
     (ATSINStmpset tmp126 (ATSPMVtysum tmp119 tmp125))
     (ATSPMVtyrec tmp124 tmp126)
    ) ;; if-then
    (begin
     (ATSPMVtyrec tmp119 tmp120)
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_exists arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret127 #f)
  (tmp128 #f)
  (tmp129 #f)
  (tmp130 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_list_exists
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg0)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_false
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp128 (ATSSELcon arg0 0))
         (ATSINStmpset tmp129 (ATSSELcon arg0 1))
         (ATSINStmpset tmp130 ((ATSfunclo_fclo arg1) arg1 tmp128))
         (if tmp130
           (begin
            atscc2scm_true
           ) ;; if-then
           (begin
            ;; apy0 = tmp129
            ;; apy1 = arg1
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; funlab_scm = 1; // __patsflab_list_exists
            (ats2scmpre_list_exists tmp129 arg1)
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret127;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_exists_method arg0)
(let(
;;knd = 0
  (tmpret131 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_exists_method
  (_ats2scmpre_list_patsfun_35__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_35 env0 arg0)
(let(
;;knd = 0
  (tmpret132 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_35
  (ats2scmpre_list_exists env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_iexists arg0 arg1)
(let(
;;knd = 0
  (tmpret133 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_iexists
  (_ats2scmpre_list_loop_37 arg1 0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop_37 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret134 #f)
  (tmp135 #f)
  (tmp136 #f)
  (tmp137 #f)
  (tmp138 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop_37
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg1)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_false
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp135 (ATSSELcon arg1 0))
         (ATSINStmpset tmp136 (ATSSELcon arg1 1))
         (ATSINStmpset tmp137 ((ATSfunclo_fclo env0) env0 arg0 tmp135))
         (if tmp137
           (begin
            atscc2scm_true
           ) ;; if-then
           (begin
            (ATSINStmpset tmp138 (ats2scmpre_add_int1_int1 arg0 1))
            ;; apy0 = tmp138
            ;; apy1 = tmp136
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop_37
            (_ats2scmpre_list_loop_37 env0 tmp138 tmp136)
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret134;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_iexists_method arg0)
(let(
;;knd = 0
  (tmpret139 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_iexists_method
  (_ats2scmpre_list_patsfun_39__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_39 env0 arg0)
(let(
;;knd = 0
  (tmpret140 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_39
  (ats2scmpre_list_iexists env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_forall arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret141 #f)
  (tmp142 #f)
  (tmp143 #f)
  (tmp144 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_list_forall
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg0)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_true
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp142 (ATSSELcon arg0 0))
         (ATSINStmpset tmp143 (ATSSELcon arg0 1))
         (ATSINStmpset tmp144 ((ATSfunclo_fclo arg1) arg1 tmp142))
         (if tmp144
           (begin
            ;; apy0 = tmp143
            ;; apy1 = arg1
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; funlab_scm = 1; // __patsflab_list_forall
            (ats2scmpre_list_forall tmp143 arg1)
           ) ;; if-then
           (begin
            atscc2scm_false
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret141;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_forall_method arg0)
(let(
;;knd = 0
  (tmpret145 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_forall_method
  (_ats2scmpre_list_patsfun_42__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_42 env0 arg0)
(let(
;;knd = 0
  (tmpret146 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_42
  (ats2scmpre_list_forall env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_iforall arg0 arg1)
(let(
;;knd = 0
  (tmpret147 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_iforall
  (_ats2scmpre_list_loop_44 arg1 0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop_44 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret148 #f)
  (tmp149 #f)
  (tmp150 #f)
  (tmp151 #f)
  (tmp152 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop_44
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg1)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_true
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp149 (ATSSELcon arg1 0))
         (ATSINStmpset tmp150 (ATSSELcon arg1 1))
         (ATSINStmpset tmp151 ((ATSfunclo_fclo env0) env0 arg0 tmp149))
         (if tmp151
           (begin
            (ATSINStmpset tmp152 (ats2scmpre_add_int1_int1 arg0 1))
            ;; apy0 = tmp152
            ;; apy1 = tmp150
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop_44
            (_ats2scmpre_list_loop_44 env0 tmp152 tmp150)
           ) ;; if-then
           (begin
            atscc2scm_false
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret148;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_iforall_method arg0)
(let(
;;knd = 0
  (tmpret153 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_iforall_method
  (_ats2scmpre_list_patsfun_46__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_46 env0 arg0)
(let(
;;knd = 0
  (tmpret154 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_46
  (ats2scmpre_list_iforall env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_app arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_app
  (ATSINSmove1_void (ats2scmpre_list_foreach arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_foreach arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmp157 #f)
  (tmp158 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_list_foreach
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg0)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         ATSINSmove0_void
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp157 (ATSSELcon arg0 0))
         (ATSINStmpset tmp158 (ATSSELcon arg0 1))
         (ATSINSmove1_void ((ATSfunclo_fclo arg1) arg1 tmp157))
         ;; apy0 = tmp158
         ;; apy1 = arg1
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab_list_foreach
         (ats2scmpre_list_foreach tmp158 arg1)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return/*_void*/;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_foreach_method arg0)
(let(
;;knd = 0
  (tmpret160 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_foreach_method
  (_ats2scmpre_list_patsfun_50__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_50 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_50
  (ATSINSmove1_void (ats2scmpre_list_foreach env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_iforeach arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_iforeach
  (ATSINSmove1_void (_ats2scmpre_list_aux_52 arg1 0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_aux_52 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmp164 #f)
  (tmp165 #f)
  (tmp167 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_aux_52
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg1)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         ATSINSmove0_void
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp164 (ATSSELcon arg1 0))
         (ATSINStmpset tmp165 (ATSSELcon arg1 1))
         (ATSINSmove1_void ((ATSfunclo_fclo env0) env0 arg0 tmp164))
         (ATSINStmpset tmp167 (ats2scmpre_add_int1_int1 arg0 1))
         ;; apy0 = tmp167
         ;; apy1 = tmp165
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_aux_52
         (_ats2scmpre_list_aux_52 env0 tmp167 tmp165)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return/*_void*/;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_iforeach_method arg0)
(let(
;;knd = 0
  (tmpret168 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_iforeach_method
  (_ats2scmpre_list_patsfun_54__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_54 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_54
  (ATSINSmove1_void (ats2scmpre_list_iforeach env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_rforeach arg0 arg1)
(let(
;;knd = 0
  (tmp171 #f)
  (tmp172 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_rforeach
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       ATSINSmove0_void
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp171 (ATSSELcon arg0 0))
       (ATSINStmpset tmp172 (ATSSELcon arg0 1))
       (ATSINSmove1_void (ats2scmpre_list_rforeach tmp172 arg1))
       (ATSINSmove1_void ((ATSfunclo_fclo arg1) arg1 tmp171))
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_rforeach_method arg0)
(let(
;;knd = 0
  (tmpret174 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_rforeach_method
  (_ats2scmpre_list_patsfun_57__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_57 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_57
  (ATSINSmove1_void (ats2scmpre_list_rforeach env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_filter arg0 arg1)
(let(
;;knd = 0
  (tmpret176 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_filter
  (_ats2scmpre_list_aux_59 arg1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_aux_59 env0 arg0)
(let(
;;knd = 1
;;(apy0 #f)
  (tmpret177 #f)
  (tmp178 #f)
  (tmp179 #f)
  (tmp180 #f)
  (tmp181 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_aux_59
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg0)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_null
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp178 (ATSSELcon arg0 0))
         (ATSINStmpset tmp179 (ATSSELcon arg0 1))
         (ATSINStmpset tmp180 ((ATSfunclo_fclo env0) env0 tmp178))
         (if tmp180
           (begin
            (ATSINStmpset tmp181 (_ats2scmpre_list_aux_59 env0 tmp179))
            (ATSPMVtysum tmp178 tmp181)
           ) ;; if-then
           (begin
            ;; apy0 = tmp179
            ;; arg0 = apy0
            ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_aux_59
            (_ats2scmpre_list_aux_59 env0 tmp179)
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret177;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_filter_method arg0)
(let(
;;knd = 0
  (tmpret182 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_filter_method
  (_ats2scmpre_list_patsfun_61__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_61 env0 arg0)
(let(
;;knd = 0
  (tmpret183 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_61
  (ats2scmpre_list_filter env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_map arg0 arg1)
(let(
;;knd = 0
  (tmpret184 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_map
  (_ats2scmpre_list_aux_63 arg1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_aux_63 env0 arg0)
(let(
;;knd = 0
  (tmpret185 #f)
  (tmp186 #f)
  (tmp187 #f)
  (tmp188 #f)
  (tmp189 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_aux_63
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp186 (ATSSELcon arg0 0))
       (ATSINStmpset tmp187 (ATSSELcon arg0 1))
       (ATSINStmpset tmp188 ((ATSfunclo_fclo env0) env0 tmp186))
       (ATSINStmpset tmp189 (_ats2scmpre_list_aux_63 env0 tmp187))
       (ATSPMVtysum tmp188 tmp189)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_map_method arg0 arg1)
(let(
;;knd = 0
  (tmpret190 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_map_method
  (_ats2scmpre_list_patsfun_65__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_65 env0 arg0)
(let(
;;knd = 0
  (tmpret191 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_65
  (ats2scmpre_list_map env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_foldleft arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret192 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_foldleft
  (_ats2scmpre_list_loop_67 arg2 arg1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop_67 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret193 #f)
  (tmp194 #f)
  (tmp195 #f)
  (tmp196 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop_67
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg1)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg0
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp194 (ATSSELcon arg1 0))
         (ATSINStmpset tmp195 (ATSSELcon arg1 1))
         (ATSINStmpset tmp196 ((ATSfunclo_fclo env0) env0 arg0 tmp194))
         ;; apy0 = tmp196
         ;; apy1 = tmp195
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop_67
         (_ats2scmpre_list_loop_67 env0 tmp196 tmp195)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret193;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_foldleft_method arg0 arg1)
(let(
;;knd = 0
  (tmpret197 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_foldleft_method
  (_ats2scmpre_list_patsfun_69__closurerize arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_69 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret198 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_69
  (ats2scmpre_list_foldleft env0 env1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_ifoldleft arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret199 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_ifoldleft
  (_ats2scmpre_list_loop_71 arg2 0 arg1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop_71 env0 arg0 arg1 arg2)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
  (tmpret200 #f)
  (tmp201 #f)
  (tmp202 #f)
  (tmp203 #f)
  (tmp204 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop_71
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg2)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg1
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp201 (ATSSELcon arg2 0))
         (ATSINStmpset tmp202 (ATSSELcon arg2 1))
         (ATSINStmpset tmp203 (ats2scmpre_add_int1_int1 arg0 1))
         (ATSINStmpset tmp204 ((ATSfunclo_fclo env0) env0 arg0 arg1 tmp201))
         ;; apy0 = tmp203
         ;; apy1 = tmp204
         ;; apy2 = tmp202
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; arg2 = apy2
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop_71
         (_ats2scmpre_list_loop_71 env0 tmp203 tmp204 tmp202)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret200;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_ifoldleft_method arg0 arg1)
(let(
;;knd = 0
  (tmpret205 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_ifoldleft_method
  (_ats2scmpre_list_patsfun_73__closurerize arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_73 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret206 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_73
  (ats2scmpre_list_ifoldleft env0 env1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_foldright arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret207 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_foldright
  (_ats2scmpre_list_aux_75 arg1 arg0 arg2)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_aux_75 env0 arg0 arg1)
(let(
;;knd = 0
  (tmpret208 #f)
  (tmp209 #f)
  (tmp210 #f)
  (tmp211 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_aux_75
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       arg1
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp209 (ATSSELcon arg0 0))
       (ATSINStmpset tmp210 (ATSSELcon arg0 1))
       (ATSINStmpset tmp211 (_ats2scmpre_list_aux_75 env0 tmp210 arg1))
       ((ATSfunclo_fclo env0) env0 tmp209 tmp211)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_foldright_method arg0 arg1)
(let(
;;knd = 0
  (tmpret212 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_foldright_method
  (_ats2scmpre_list_patsfun_77__closurerize arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_77 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret213 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_77
  (ats2scmpre_list_foldright env0 arg0 env1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_ifoldright arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret214 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_ifoldright
  (_ats2scmpre_list_aux_79 arg1 0 arg0 arg2)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_aux_79 env0 arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret215 #f)
  (tmp216 #f)
  (tmp217 #f)
  (tmp218 #f)
  (tmp219 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_aux_79
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg1)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       arg2
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp216 (ATSSELcon arg1 0))
       (ATSINStmpset tmp217 (ATSSELcon arg1 1))
       (ATSINStmpset tmp219 (ats2scmpre_add_int1_int1 arg0 1))
       (ATSINStmpset tmp218 (_ats2scmpre_list_aux_79 env0 tmp219 tmp217 arg2))
       ((ATSfunclo_fclo env0) env0 arg0 tmp216 tmp218)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_ifoldright_method arg0 arg1)
(let(
;;knd = 0
  (tmpret220 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_ifoldright_method
  (_ats2scmpre_list_patsfun_81__closurerize arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_81 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret221 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_81
  (ats2scmpre_list_ifoldright env0 arg0 env1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_streamize_list_elt arg0)
(let(
;;knd = 0
  (tmpret224 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_streamize_list_elt
  (_ats2scmpre_list_auxmain_85 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_auxmain_85 arg0)
(let(
;;knd = 0
  (tmpret225 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_auxmain_85
  (ATSPMVllazyval (_ats2scmpre_list_patsfun_86__closurerize arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_86 env0 arg0)
(let(
;;knd = 0
  (tmpret226 #f)
  (tmp227 #f)
  (tmp228 #f)
  (tmp229 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_86
  (if arg0
    (begin
     (letrec(
       (casefnx
        (lambda(tmplab)(case tmplab
         ;; ATSbranchseq_beg
         ((1)
          (if (ATSCKptriscons env0)
            (casefnx 4)
            (begin
             (casefnx 2)
            )
          )
         ) ;; end-of-branch
         ((2)
          atscc2scm_null
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; ATSbranchseq_beg
         ((3)
          (casefnx 4)
         ) ;; end-of-branch
         ((4)
          (ATSINStmpset tmp227 (ATSSELcon env0 0))
          (ATSINStmpset tmp228 (ATSSELcon env0 1))
          (ATSINStmpset tmp229 (_ats2scmpre_list_auxmain_85 tmp228))
          (ATSPMVtysum tmp227 tmp229)
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
         ) ;; end-of-case
        ) ;; end-of-lambda
       ) ;; end-of-casefnx
      ) (casefnx 1)
     ) ;; end-of-letrec
    ) ;; if-then
    (begin
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_streamize_list_zip arg0 arg1)
(let(
;;knd = 0
  (tmpret230 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_streamize_list_zip
  (_ats2scmpre_list_auxmain_88 arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_auxmain_88 arg0 arg1)
(let(
;;knd = 0
  (tmpret231 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_auxmain_88
  (ATSPMVllazyval (_ats2scmpre_list_patsfun_89__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_89 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret232 #f)
  (tmp233 #f)
  (tmp234 #f)
  (tmp235 #f)
  (tmp236 #f)
  (tmp237 #f)
  (tmp238 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_89
  (if arg0
    (begin
     (letrec(
       (casefnx
        (lambda(tmplab)(case tmplab
         ;; ATSbranchseq_beg
         ((1)
          (if (ATSCKptriscons env0)
            (casefnx 4)
            (begin
             (casefnx 2)
            )
          )
         ) ;; end-of-branch
         ((2)
          atscc2scm_null
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; ATSbranchseq_beg
         ((3)
          (casefnx 4)
         ) ;; end-of-branch
         ((4)
          (ATSINStmpset tmp233 (ATSSELcon env0 0))
          (ATSINStmpset tmp234 (ATSSELcon env0 1))
          (letrec(
            (casefnx
             (lambda(tmplab)(case tmplab
              ;; ATSbranchseq_beg
              ((1)
               (if (ATSCKptriscons env1)
                 (casefnx 4)
                 (begin
                  (casefnx 2)
                 )
               )
              ) ;; end-of-branch
              ((2)
               atscc2scm_null
              ) ;; end-of-branch
              ;; ATSbranchseq_end
              ;; ATSbranchseq_beg
              ((3)
               (casefnx 4)
              ) ;; end-of-branch
              ((4)
               (ATSINStmpset tmp235 (ATSSELcon env1 0))
               (ATSINStmpset tmp236 (ATSSELcon env1 1))
               (ATSINStmpset tmp237 (ATSPMVtyrec tmp233 tmp235))
               (ATSINStmpset tmp238 (_ats2scmpre_list_auxmain_88 tmp234 tmp236))
               (ATSPMVtysum tmp237 tmp238)
              ) ;; end-of-branch
              ;; ATSbranchseq_end
              ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
              ) ;; end-of-case
             ) ;; end-of-lambda
            ) ;; end-of-casefnx
           ) (casefnx 1)
          ) ;; end-of-letrec
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
         ) ;; end-of-case
        ) ;; end-of-lambda
       ) ;; end-of-casefnx
      ) (casefnx 1)
     ) ;; end-of-letrec
    ) ;; if-then
    (begin
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_streamize_list_cross arg0 arg1)
(let(
;;knd = 0
  (tmpret239 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_streamize_list_cross
  (_ats2scmpre_list_auxmain_93 arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_auxone_91 arg0 arg1)
(let(
;;knd = 0
  (tmpret240 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_auxone_91
  (ATSPMVllazyval (_ats2scmpre_list_patsfun_92__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_92 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret241 #f)
  (tmp242 #f)
  (tmp243 #f)
  (tmp244 #f)
  (tmp245 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_92
  (if arg0
    (begin
     (letrec(
       (casefnx
        (lambda(tmplab)(case tmplab
         ;; ATSbranchseq_beg
         ((1)
          (if (ATSCKptriscons env1)
            (casefnx 4)
            (begin
             (casefnx 2)
            )
          )
         ) ;; end-of-branch
         ((2)
          atscc2scm_null
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; ATSbranchseq_beg
         ((3)
          (casefnx 4)
         ) ;; end-of-branch
         ((4)
          (ATSINStmpset tmp242 (ATSSELcon env1 0))
          (ATSINStmpset tmp243 (ATSSELcon env1 1))
          (ATSINStmpset tmp244 (ATSPMVtyrec env0 tmp242))
          (ATSINStmpset tmp245 (_ats2scmpre_list_auxone_91 env0 tmp243))
          (ATSPMVtysum tmp244 tmp245)
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
         ) ;; end-of-case
        ) ;; end-of-lambda
       ) ;; end-of-casefnx
      ) (casefnx 1)
     ) ;; end-of-letrec
    ) ;; if-then
    (begin
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_auxmain_93 arg0 arg1)
(let(
;;knd = 0
  (tmpret246 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_auxmain_93
  (ATSPMVllazyval (_ats2scmpre_list_patsfun_94__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_94 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret247 #f)
  (tmp248 #f)
  (tmp249 #f)
  (tmp250 #f)
  (tmp251 #f)
  (tmp252 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_94
  (if arg0
    (begin
     (letrec(
       (casefnx
        (lambda(tmplab)(case tmplab
         ;; ATSbranchseq_beg
         ((1)
          (if (ATSCKptriscons env0)
            (casefnx 4)
            (begin
             (casefnx 2)
            )
          )
         ) ;; end-of-branch
         ((2)
          atscc2scm_null
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; ATSbranchseq_beg
         ((3)
          (casefnx 4)
         ) ;; end-of-branch
         ((4)
          (ATSINStmpset tmp248 (ATSSELcon env0 0))
          (ATSINStmpset tmp249 (ATSSELcon env0 1))
          (ATSINStmpset tmp251 (_ats2scmpre_list_auxone_91 tmp248 env1))
          (ATSINStmpset tmp252 (_ats2scmpre_list_auxmain_93 tmp249 env1))
          (ATSINStmpset tmp250 (ats2scmpre_stream_vt_append tmp251 tmp252))
          (ATSPMVllazyval_eval tmp250)
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
         ) ;; end-of-case
        ) ;; end-of-lambda
       ) ;; end-of-casefnx
      ) (casefnx 1)
     ) ;; end-of-letrec
    ) ;; if-then
    (begin
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_SCMlist2list_rev arg0)
(let(
;;knd = 0
  (tmpret257 #f)
  (tmp263 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_SCMlist2list_rev
  (ATSINStmpset tmp263 atscc2scm_null)
  (_ats2scmpre_list_loop_98 arg0 tmp263)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_loop_98 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret258 #f)
  (tmp259 #f)
  (tmp260 #f)
  (tmp261 #f)
  (tmp262 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_loop_98
    (ATSINStmpset tmp259 (ats2scmpre_SCMlist_is_nil arg0))
    (if tmp259
      (begin
       arg1
      ) ;; if-then
      (begin
       (ATSINStmpset tmp260 (ats2scmpre_SCMlist_tail arg0))
       (ATSINStmpset tmp262 (ats2scmpre_SCMlist_head arg0))
       (ATSINStmpset tmp261 (ATSPMVtysum tmp262 arg1))
       ;; apy0 = tmp260
       ;; apy1 = tmp261
       ;; arg0 = apy0
       ;; arg1 = apy1
       ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_loop_98
       (_ats2scmpre_list_loop_98 tmp260 tmp261)
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret258;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_SCMlist_oflist_rev arg0)
(let(
;;knd = 0
  (tmpret264 #f)
  (tmp269 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_SCMlist_oflist_rev
  (ATSINStmpset tmp269 (ats2scmpre_SCMlist_nil))
  (_ats2scmpre_list_aux_100 arg0 tmp269)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_aux_100 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret265 #f)
  (tmp266 #f)
  (tmp267 #f)
  (tmp268 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_list_aux_100
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg0)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg1
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp266 (ATSSELcon arg0 0))
         (ATSINStmpset tmp267 (ATSSELcon arg0 1))
         (ATSINStmpset tmp268 (ats2scmpre_SCMlist_cons tmp266 arg1))
         ;; apy0 = tmp267
         ;; apy1 = tmp268
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_list_aux_100
         (_ats2scmpre_list_aux_100 tmp267 tmp268)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret265;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_list_sort_2 arg0 arg1)
(let(
;;knd = 0
  (tmpret270 #f)
  (tmp271 #f)
  (tmp272 #f)
  (tmp275 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list_sort_2
  (ATSINStmpset tmp271 (ats2scmpre_SCMlist_oflist_rev arg0))
  (ATSINStmpset tmp272 (ats2scmpre_SCMlist_sort_2 tmp271 (_ats2scmpre_list_patsfun_102__closurerize arg1)))
  (ATSINStmpset tmp275 (ats2scmpre_SCMlist2list_rev tmp272))
  tmp275
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_list_patsfun_102 env0 arg0 arg1)
(let(
;;knd = 0
  (tmpret273 #f)
  (tmp274 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_list_patsfun_102
  (ATSINStmpset tmp274 ((ATSfunclo_fclo env0) env0 arg0 arg1))
  (ats2scmpre_neg_int0 tmp274)
) ;; end-of-let
) ;; end-of-fun

;;;;;;
;;
;; end-of-compilation-unit
;;
;;;;;;
;;;;;;
;;
;; The Scheme code is generated by atscc2scm
;; The starting compilation time is: 2017-4-12:  0h:35m
;;
;;;;;;

;;fun
(define
(ats2scmpre_option_some arg0)
(let(
;;knd = 0
  (tmpret0 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_option_some
  (ATSPMVtysum arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_option_none)
(let(
;;knd = 0
  (tmpret1 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_option_none
  atscc2scm_null
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_option_unsome arg0)
(let(
;;knd = 0
  (tmpret2 #f)
  (tmp3 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_option_unsome
  (ATSINStmpset tmp3 (ATSSELcon arg0 0))
  tmp3
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_option_is_some arg0)
(let(
;;knd = 0
  (tmpret4 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_option_is_some
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptrisnull arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_true
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       atscc2scm_false
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_option_is_none arg0)
(let(
;;knd = 0
  (tmpret5 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_option_is_none
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_true
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       atscc2scm_false
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun

;;;;;;
;;
;; end-of-compilation-unit
;;
;;;;;;
;;;;;;
;;
;; The Scheme code is generated by atscc2scm
;; The starting compilation time is: 2017-4-12:  0h:35m
;;
;;;;;;

(define
(_ats2scmpre_stream_patsfun_6__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_6 (ATSCCget_1 _fcenvs_))) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_17__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_patsfun_17 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_23__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_23 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_))) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_25__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_25 (ATSCCget_1 _fcenvs_))) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_27__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_27 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_))) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_29__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_patsfun_29 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_31__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_31 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_))) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_33__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_patsfun_33 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_36__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_patsfun_36 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_39__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_patsfun_39 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_42__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_patsfun_42 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_46__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_patsfun_46 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_49__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_49 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_))) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_52__closurerize xenv0 xenv1 xenv2 xenv3)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_52 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) (ATSCCget_3 _fcenvs_) (ATSCCget_at _fcenvs_ 4))) xenv0 xenv1 xenv2 xenv3)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_53__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_53 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_))) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_56__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_56 (ATSCCget_1 _fcenvs_))) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_58__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_58 (ATSCCget_1 _fcenvs_))) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_60__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_60 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_))) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_65__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0 xarg1)(_ats2scmpre_stream_patsfun_65 (ATSCCget_1 _fcenvs_) xarg0 xarg1)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_67__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0 xarg1)(_ats2scmpre_stream_patsfun_67 (ATSCCget_1 _fcenvs_) xarg0 xarg1)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_70__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_70 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_))) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_patsfun_72__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_patsfun_72 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_))) xenv0 xenv1)
;;%}
) ;; define


;;fun
(define
(ats2scmpre_stream_make_list arg0)
(let(
;;knd = 0
  (tmpret7 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_make_list
  (ATSPMVlazyval (_ats2scmpre_stream_patsfun_6__closurerize arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_6 env0)
(let(
;;knd = 0
  (tmpret8 #f)
  (tmp9 #f)
  (tmp10 #f)
  (tmp11 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_6
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons env0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp9 (ATSSELcon env0 0))
       (ATSINStmpset tmp10 (ATSSELcon env0 1))
       (ATSINStmpset tmp11 (ats2scmpre_stream_make_list tmp10))
       (ATSPMVtysum tmp9 tmp11)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_make_list0 arg0)
(let(
;;knd = 0
  (tmpret12 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_make_list0
  (ats2scmpre_stream_make_list arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_nth_opt arg0 arg1)
(let(
;;knd = 0
  (tmpret13 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_nth_opt
  (_ats2scmpre_stream_loop_9 arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_loop_9 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret14 #f)
  (tmp15 #f)
  (tmp16 #f)
  (tmp17 #f)
  (tmp18 #f)
  (tmp19 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_loop_9
    (ATSINStmpset tmp15 (ATSPMVlazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp15)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_null
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp16 (ATSSELcon tmp15 0))
         (ATSINStmpset tmp17 (ATSSELcon tmp15 1))
         (ATSINStmpset tmp18 (ats2scmpre_gt_int1_int1 arg1 0))
         (if tmp18
           (begin
            (ATSINStmpset tmp19 (ats2scmpre_pred_int1 arg1))
            ;; apy0 = tmp17
            ;; apy1 = tmp19
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_loop_9
            (_ats2scmpre_stream_loop_9 tmp17 tmp19)
           ) ;; if-then
           (begin
            (ATSPMVtysum tmp16)
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret14;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_length arg0)
(let(
;;knd = 0
  (tmpret20 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_length
  (_ats2scmpre_stream_loop_11 arg0 0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_loop_11 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret21 #f)
  (tmp22 #f)
  (tmp24 #f)
  (tmp25 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_loop_11
    (ATSINStmpset tmp22 (ATSPMVlazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp22)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg1
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp24 (ATSSELcon tmp22 1))
         (ATSINStmpset tmp25 (ats2scmpre_add_int1_int1 arg1 1))
         ;; apy0 = tmp24
         ;; apy1 = tmp25
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_loop_11
         (_ats2scmpre_stream_loop_11 tmp24 tmp25)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret21;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream2list arg0)
(let(
;;knd = 0
  (tmpret26 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream2list
  (_ats2scmpre_stream_aux_13 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_aux_13 arg0)
(let(
;;knd = 0
  (tmpret27 #f)
  (tmp28 #f)
  (tmp29 #f)
  (tmp30 #f)
  (tmp31 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_aux_13
  (ATSINStmpset tmp28 (ATSPMVlazyval_eval arg0))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp28)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp29 (ATSSELcon tmp28 0))
       (ATSINStmpset tmp30 (ATSSELcon tmp28 1))
       (ATSINStmpset tmp31 (_ats2scmpre_stream_aux_13 tmp30))
       (ATSPMVtysum tmp29 tmp31)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream2list_rev arg0)
(let(
;;knd = 0
  (tmpret32 #f)
  (tmp38 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream2list_rev
  (ATSINStmpset tmp38 atscc2scm_null)
  (_ats2scmpre_stream_loop_15 arg0 tmp38)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_loop_15 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret33 #f)
  (tmp34 #f)
  (tmp35 #f)
  (tmp36 #f)
  (tmp37 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_loop_15
    (ATSINStmpset tmp34 (ATSPMVlazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp34)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg1
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp35 (ATSSELcon tmp34 0))
         (ATSINStmpset tmp36 (ATSSELcon tmp34 1))
         (ATSINStmpset tmp37 (ATSPMVtysum tmp35 arg1))
         ;; apy0 = tmp36
         ;; apy1 = tmp37
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_loop_15
         (_ats2scmpre_stream_loop_15 tmp36 tmp37)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret33;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_takeLte arg0 arg1)
(let(
;;knd = 0
  (tmpret39 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_takeLte
  (ATSPMVllazyval (_ats2scmpre_stream_patsfun_17__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_17 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret40 #f)
  (tmp41 #f)
  (tmp42 #f)
  (tmp43 #f)
  (tmp44 #f)
  (tmp45 #f)
  (tmp46 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_17
  (if arg0
    (begin
     (ATSINStmpset tmp41 (ats2scmpre_gt_int1_int1 env1 0))
     (if tmp41
       (begin
        (ATSINStmpset tmp42 (ATSPMVlazyval_eval env0))
        (letrec(
          (casefnx
           (lambda(tmplab)(case tmplab
            ;; ATSbranchseq_beg
            ((1)
             (if (ATSCKptriscons tmp42)
               (casefnx 4)
               (begin
                (casefnx 2)
               )
             )
            ) ;; end-of-branch
            ((2)
             atscc2scm_null
            ) ;; end-of-branch
            ;; ATSbranchseq_end
            ;; ATSbranchseq_beg
            ((3)
             (casefnx 4)
            ) ;; end-of-branch
            ((4)
             (ATSINStmpset tmp43 (ATSSELcon tmp42 0))
             (ATSINStmpset tmp44 (ATSSELcon tmp42 1))
             (ATSINStmpset tmp46 (ats2scmpre_sub_int1_int1 env1 1))
             (ATSINStmpset tmp45 (ats2scmpre_stream_takeLte tmp44 tmp46))
             (ATSPMVtysum tmp43 tmp45)
            ) ;; end-of-branch
            ;; ATSbranchseq_end
            ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
            ) ;; end-of-case
           ) ;; end-of-lambda
          ) ;; end-of-casefnx
         ) (casefnx 1)
        ) ;; end-of-letrec
       ) ;; if-then
       (begin
        atscc2scm_null
       ) ;; if-else
     )
    ) ;; if-then
    (begin
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_take_opt arg0 arg1)
(let(
;;knd = 0
  (tmpret47 #f)
  (tmp56 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_take_opt
  (ATSINStmpset tmp56 atscc2scm_null)
  (_ats2scmpre_stream_auxmain_19 arg1 arg0 0 tmp56)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_auxmain_19 env0 arg0 arg1 arg2)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
  (tmpret48 #f)
  (tmp49 #f)
  (tmp50 #f)
  (tmp51 #f)
  (tmp52 #f)
  (tmp53 #f)
  (tmp54 #f)
  (tmp55 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_auxmain_19
    (ATSINStmpset tmp49 (ats2scmpre_lt_int1_int1 arg1 env0))
    (if tmp49
      (begin
       (ATSINStmpset tmp50 (ATSPMVlazyval_eval arg0))
       (letrec(
         (casefnx
          (lambda(tmplab)(case tmplab
           ;; ATSbranchseq_beg
           ((1)
            (if (ATSCKptriscons tmp50)
              (casefnx 4)
              (begin
               (casefnx 2)
              )
            )
           ) ;; end-of-branch
           ((2)
            atscc2scm_null
           ) ;; end-of-branch
           ;; ATSbranchseq_end
           ;; ATSbranchseq_beg
           ((3)
            (casefnx 4)
           ) ;; end-of-branch
           ((4)
            (ATSINStmpset tmp51 (ATSSELcon tmp50 0))
            (ATSINStmpset tmp52 (ATSSELcon tmp50 1))
            (ATSINStmpset tmp53 (ats2scmpre_add_int1_int1 arg1 1))
            (ATSINStmpset tmp54 (ATSPMVtysum tmp51 arg2))
            ;; apy0 = tmp52
            ;; apy1 = tmp53
            ;; apy2 = tmp54
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; arg2 = apy2
            ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_auxmain_19
            (_ats2scmpre_stream_auxmain_19 env0 tmp52 tmp53 tmp54)
           ) ;; end-of-branch
           ;; ATSbranchseq_end
           ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
           ) ;; end-of-case
          ) ;; end-of-lambda
         ) ;; end-of-casefnx
        ) (casefnx 1)
       ) ;; end-of-letrec
      ) ;; if-then
      (begin
       (ATSINStmpset tmp55 (ats2scmpre_list_reverse arg2))
       (ATSPMVtysum tmp55)
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret48;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_drop_opt arg0 arg1)
(let(
;;knd = 0
  (tmpret57 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_drop_opt
  (_ats2scmpre_stream_auxmain_21 arg1 arg0 0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_auxmain_21 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret58 #f)
  (tmp59 #f)
  (tmp60 #f)
  (tmp62 #f)
  (tmp63 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_auxmain_21
    (ATSINStmpset tmp59 (ats2scmpre_lt_int1_int1 arg1 env0))
    (if tmp59
      (begin
       (ATSINStmpset tmp60 (ATSPMVlazyval_eval arg0))
       (letrec(
         (casefnx
          (lambda(tmplab)(case tmplab
           ;; ATSbranchseq_beg
           ((1)
            (if (ATSCKptriscons tmp60)
              (casefnx 4)
              (begin
               (casefnx 2)
              )
            )
           ) ;; end-of-branch
           ((2)
            atscc2scm_null
           ) ;; end-of-branch
           ;; ATSbranchseq_end
           ;; ATSbranchseq_beg
           ((3)
            (casefnx 4)
           ) ;; end-of-branch
           ((4)
            (ATSINStmpset tmp62 (ATSSELcon tmp60 1))
            (ATSINStmpset tmp63 (ats2scmpre_add_int1_int1 arg1 1))
            ;; apy0 = tmp62
            ;; apy1 = tmp63
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_auxmain_21
            (_ats2scmpre_stream_auxmain_21 env0 tmp62 tmp63)
           ) ;; end-of-branch
           ;; ATSbranchseq_end
           ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
           ) ;; end-of-case
          ) ;; end-of-lambda
         ) ;; end-of-casefnx
        ) (casefnx 1)
       ) ;; end-of-letrec
      ) ;; if-then
      (begin
       (ATSPMVtysum arg0)
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret58;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_append arg0 arg1)
(let(
;;knd = 0
  (tmpret64 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_append
  (ATSPMVlazyval (_ats2scmpre_stream_patsfun_23__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_23 env0 env1)
(let(
;;knd = 0
  (tmpret65 #f)
  (tmp66 #f)
  (tmp67 #f)
  (tmp68 #f)
  (tmp69 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_23
  (ATSINStmpset tmp66 (ATSPMVlazyval_eval env0))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp66)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       (ATSPMVlazyval_eval env1)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp67 (ATSSELcon tmp66 0))
       (ATSINStmpset tmp68 (ATSSELcon tmp66 1))
       (ATSINStmpset tmp69 (ats2scmpre_stream_append tmp68 env1))
       (ATSPMVtysum tmp67 tmp69)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_concat arg0)
(let(
;;knd = 0
  (tmpret70 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_concat
  (ATSPMVlazyval (_ats2scmpre_stream_patsfun_25__closurerize arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_25 env0)
(let(
;;knd = 0
  (tmpret71 #f)
  (tmp72 #f)
  (tmp73 #f)
  (tmp74 #f)
  (tmp75 #f)
  (tmp76 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_25
  (ATSINStmpset tmp72 (ATSPMVlazyval_eval env0))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp72)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp73 (ATSSELcon tmp72 0))
       (ATSINStmpset tmp74 (ATSSELcon tmp72 1))
       (ATSINStmpset tmp76 (ats2scmpre_stream_concat tmp74))
       (ATSINStmpset tmp75 (ats2scmpre_stream_append tmp73 tmp76))
       (ATSPMVlazyval_eval tmp75)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_map_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret77 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_map_cloref
  (ATSPMVlazyval (_ats2scmpre_stream_patsfun_27__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_27 env0 env1)
(let(
;;knd = 0
  (tmpret78 #f)
  (tmp79 #f)
  (tmp80 #f)
  (tmp81 #f)
  (tmp82 #f)
  (tmp83 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_27
  (ATSINStmpset tmp79 (ATSPMVlazyval_eval env0))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp79)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp80 (ATSSELcon tmp79 0))
       (ATSINStmpset tmp81 (ATSSELcon tmp79 1))
       (ATSINStmpset tmp82 ((ATSfunclo_fclo env1) env1 tmp80))
       (ATSINStmpset tmp83 (ats2scmpre_stream_map_cloref tmp81 env1))
       (ATSPMVtysum tmp82 tmp83)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_map_method arg0 arg1)
(let(
;;knd = 0
  (tmpret84 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_map_method
  (_ats2scmpre_stream_patsfun_29__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_29 env0 arg0)
(let(
;;knd = 0
  (tmpret85 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_29
  (ats2scmpre_stream_map_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_filter_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret86 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_filter_cloref
  (ATSPMVlazyval (_ats2scmpre_stream_patsfun_31__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_31 env0 env1)
(let(
;;knd = 0
  (tmpret87 #f)
  (tmp88 #f)
  (tmp89 #f)
  (tmp90 #f)
  (tmp91 #f)
  (tmp92 #f)
  (tmp93 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_31
  (ATSINStmpset tmp88 (ATSPMVlazyval_eval env0))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp88)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp89 (ATSSELcon tmp88 0))
       (ATSINStmpset tmp90 (ATSSELcon tmp88 1))
       (ATSINStmpset tmp91 ((ATSfunclo_fclo env1) env1 tmp89))
       (if tmp91
         (begin
          (ATSINStmpset tmp92 (ats2scmpre_stream_filter_cloref tmp90 env1))
          (ATSPMVtysum tmp89 tmp92)
         ) ;; if-then
         (begin
          (ATSINStmpset tmp93 (ats2scmpre_stream_filter_cloref tmp90 env1))
          (ATSPMVlazyval_eval tmp93)
         ) ;; if-else
       )
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_filter_method arg0)
(let(
;;knd = 0
  (tmpret94 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_filter_method
  (_ats2scmpre_stream_patsfun_33__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_33 env0 arg0)
(let(
;;knd = 0
  (tmpret95 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_33
  (ats2scmpre_stream_filter_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_forall_cloref arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret96 #f)
  (tmp97 #f)
  (tmp98 #f)
  (tmp99 #f)
  (tmp100 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_stream_forall_cloref
    (ATSINStmpset tmp97 (ATSPMVlazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp97)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_true
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp98 (ATSSELcon tmp97 0))
         (ATSINStmpset tmp99 (ATSSELcon tmp97 1))
         (ATSINStmpset tmp100 ((ATSfunclo_fclo arg1) arg1 tmp98))
         (if tmp100
           (begin
            ;; apy0 = tmp99
            ;; apy1 = arg1
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; funlab_scm = 1; // __patsflab_stream_forall_cloref
            (ats2scmpre_stream_forall_cloref tmp99 arg1)
           ) ;; if-then
           (begin
            atscc2scm_false
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret96;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_forall_method arg0)
(let(
;;knd = 0
  (tmpret101 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_forall_method
  (_ats2scmpre_stream_patsfun_36__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_36 env0 arg0)
(let(
;;knd = 0
  (tmpret102 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_36
  (ats2scmpre_stream_forall_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_exists_cloref arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret103 #f)
  (tmp104 #f)
  (tmp105 #f)
  (tmp106 #f)
  (tmp107 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_stream_exists_cloref
    (ATSINStmpset tmp104 (ATSPMVlazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp104)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_false
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp105 (ATSSELcon tmp104 0))
         (ATSINStmpset tmp106 (ATSSELcon tmp104 1))
         (ATSINStmpset tmp107 ((ATSfunclo_fclo arg1) arg1 tmp105))
         (if tmp107
           (begin
            atscc2scm_true
           ) ;; if-then
           (begin
            ;; apy0 = tmp106
            ;; apy1 = arg1
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; funlab_scm = 1; // __patsflab_stream_exists_cloref
            (ats2scmpre_stream_exists_cloref tmp106 arg1)
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret103;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_exists_method arg0)
(let(
;;knd = 0
  (tmpret108 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_exists_method
  (_ats2scmpre_stream_patsfun_39__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_39 env0 arg0)
(let(
;;knd = 0
  (tmpret109 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_39
  (ats2scmpre_stream_exists_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_foreach_cloref arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmp111 #f)
  (tmp112 #f)
  (tmp113 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_stream_foreach_cloref
    (ATSINStmpset tmp111 (ATSPMVlazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp111)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         ATSINSmove0_void
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp112 (ATSSELcon tmp111 0))
         (ATSINStmpset tmp113 (ATSSELcon tmp111 1))
         (ATSINSmove1_void ((ATSfunclo_fclo arg1) arg1 tmp112))
         ;; apy0 = tmp113
         ;; apy1 = arg1
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab_stream_foreach_cloref
         (ats2scmpre_stream_foreach_cloref tmp113 arg1)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return/*_void*/;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_foreach_method arg0)
(let(
;;knd = 0
  (tmpret115 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_foreach_method
  (_ats2scmpre_stream_patsfun_42__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_42 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_42
  (ATSINSmove1_void (ats2scmpre_stream_foreach_cloref env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_iforeach_cloref arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_iforeach_cloref
  (ATSINSmove1_void (_ats2scmpre_stream_loop_44 arg1 0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_loop_44 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmp119 #f)
  (tmp120 #f)
  (tmp121 #f)
  (tmp123 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_loop_44
    (ATSINStmpset tmp119 (ATSPMVlazyval_eval arg1))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp119)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         ATSINSmove0_void
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp120 (ATSSELcon tmp119 0))
         (ATSINStmpset tmp121 (ATSSELcon tmp119 1))
         (ATSINSmove1_void ((ATSfunclo_fclo env0) env0 arg0 tmp120))
         (ATSINStmpset tmp123 (ats2scmpre_add_int1_int1 arg0 1))
         ;; apy0 = tmp123
         ;; apy1 = tmp121
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_loop_44
         (_ats2scmpre_stream_loop_44 env0 tmp123 tmp121)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return/*_void*/;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_iforeach_method arg0)
(let(
;;knd = 0
  (tmpret124 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_iforeach_method
  (_ats2scmpre_stream_patsfun_46__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_46 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_46
  (ATSINSmove1_void (ats2scmpre_stream_iforeach_cloref env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_tabulate_cloref arg0)
(let(
;;knd = 0
  (tmpret126 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_tabulate_cloref
  (_ats2scmpre_stream_auxmain_48 arg0 0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_auxmain_48 env0 arg0)
(let(
;;knd = 0
  (tmpret127 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_auxmain_48
  (ATSPMVlazyval (_ats2scmpre_stream_patsfun_49__closurerize env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_49 env0 env1)
(let(
;;knd = 0
  (tmpret128 #f)
  (tmp129 #f)
  (tmp130 #f)
  (tmp131 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_49
  (ATSINStmpset tmp129 ((ATSfunclo_fclo env0) env0 env1))
  (ATSINStmpset tmp131 (ats2scmpre_add_int1_int1 env1 1))
  (ATSINStmpset tmp130 (_ats2scmpre_stream_auxmain_48 env0 tmp131))
  (ATSPMVtysum tmp129 tmp130)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_cross_stream_list arg0 arg1)
(let(
;;knd = 0
  (tmpret132 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_cross_stream_list
  (ATSPMVlazyval (_ats2scmpre_stream_patsfun_53__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_auxmain_51 arg0 arg1 arg2 arg3)
(let(
;;knd = 0
  (tmpret133 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_auxmain_51
  (ATSPMVlazyval (_ats2scmpre_stream_patsfun_52__closurerize arg0 arg1 arg2 arg3))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_52 env0 env1 env2 env3)
(let(
;;knd = 0
  (tmpret134 #f)
  (tmp135 #f)
  (tmp136 #f)
  (tmp137 #f)
  (tmp138 #f)
  (tmp139 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_52
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons env3)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       (ATSINStmpset tmp137 (ats2scmpre_cross_stream_list env1 env2))
       (ATSPMVlazyval_eval tmp137)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp135 (ATSSELcon env3 0))
       (ATSINStmpset tmp136 (ATSSELcon env3 1))
       (ATSINStmpset tmp138 (ATSPMVtyrec env0 tmp135))
       (ATSINStmpset tmp139 (_ats2scmpre_stream_auxmain_51 env0 env1 env2 tmp136))
       (ATSPMVtysum tmp138 tmp139)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_53 env0 env1)
(let(
;;knd = 0
  (tmpret140 #f)
  (tmp141 #f)
  (tmp142 #f)
  (tmp143 #f)
  (tmp144 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_53
  (ATSINStmpset tmp141 (ATSPMVlazyval_eval env0))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp141)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (if (ATSCKptrisnull tmp141)
         (ATSINScaseof_fail "/home/hwxi/Research/ATS-Postiats/contrib/libatscc/ATS2-0.3.2/DATS/stream.dats: 6907(line=451, offs=1) -- 6999(line=453, offs=50)")
         (begin
          (casefnx 4)
         )
       )
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp142 (ATSSELcon tmp141 0))
       (ATSINStmpset tmp143 (ATSSELcon tmp141 1))
       (ATSINStmpset tmp144 (_ats2scmpre_stream_auxmain_51 tmp142 tmp143 env1 env1))
       (ATSPMVlazyval_eval tmp144)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_cross_stream_list0 arg0 arg1)
(let(
;;knd = 0
  (tmpret145 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_cross_stream_list0
  (ats2scmpre_cross_stream_list arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream2cloref_exn arg0)
(let(
;;knd = 0
  (tmpret146 #f)
  (tmp147 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream2cloref_exn
  (ATSINStmpset tmp147 (ats2scmpre_ref arg0))
  (_ats2scmpre_stream_patsfun_56__closurerize tmp147)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_56 env0)
(let(
;;knd = 0
  (tmpret148 #f)
  (tmp149 #f)
  (tmp150 #f)
  (tmp151 #f)
  (tmp152 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_56
  (ATSINStmpset tmp149 (ats2scmpre_ref_get_elt env0))
  (ATSINStmpset tmp150 (ATSPMVlazyval_eval tmp149))
  (if (ATSCKptrisnull tmp150) (ATSINScaseof_fail "/home/hwxi/Research/ATS-Postiats/contrib/libatscc/ATS2-0.3.2/DATS/stream.dats: 7300(line=479, offs=5) -- 7324(line=479, offs=29)"))
  (ATSINStmpset tmp151 (ATSSELcon tmp150 0))
  (ATSINStmpset tmp152 (ATSSELcon tmp150 1))
  (ATSINSmove1_void (ats2scmpre_ref_set_elt env0 tmp152))
  tmp151
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream2cloref_opt arg0)
(let(
;;knd = 0
  (tmpret154 #f)
  (tmp155 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream2cloref_opt
  (ATSINStmpset tmp155 (ats2scmpre_ref arg0))
  (_ats2scmpre_stream_patsfun_58__closurerize tmp155)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_58 env0)
(let(
;;knd = 0
  (tmpret156 #f)
  (tmp157 #f)
  (tmp158 #f)
  (tmp159 #f)
  (tmp160 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_58
  (ATSINStmpset tmp157 (ats2scmpre_ref_get_elt env0))
  (ATSINStmpset tmp158 (ATSPMVlazyval_eval tmp157))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp158)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp159 (ATSSELcon tmp158 0))
       (ATSINStmpset tmp160 (ATSSELcon tmp158 1))
       (ATSINSmove1_void (ats2scmpre_ref_set_elt env0 tmp160))
       (ATSPMVtysum tmp159)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream2cloref_last arg0 arg1)
(let(
;;knd = 0
  (tmpret162 #f)
  (tmp163 #f)
  (tmp164 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream2cloref_last
  (ATSINStmpset tmp163 (ats2scmpre_ref arg0))
  (ATSINStmpset tmp164 (ats2scmpre_ref arg1))
  (_ats2scmpre_stream_patsfun_60__closurerize tmp163 tmp164)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_60 env0 env1)
(let(
;;knd = 0
  (tmpret165 #f)
  (tmp166 #f)
  (tmp167 #f)
  (tmp168 #f)
  (tmp169 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_60
  (ATSINStmpset tmp166 (ats2scmpre_ref_get_elt env0))
  (ATSINStmpset tmp167 (ATSPMVlazyval_eval tmp166))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp167)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       (ats2scmpre_ref_get_elt env1)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp168 (ATSSELcon tmp167 0))
       (ATSINStmpset tmp169 (ATSSELcon tmp167 1))
       (ATSINSmove1_void (ats2scmpre_ref_set_elt env0 tmp169))
       (ATSINSmove1_void (ats2scmpre_ref_set_elt env1 tmp168))
       tmp168
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_take_while_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret172 #f)
  (tmp173 #f)
  (tmp174 #f)
  (tmp175 #f)
  (tmp176 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_take_while_cloref
  (ATSINStmpset tmp173 (ats2scmpre_stream_rtake_while_cloref arg0 arg1))
  (ATSINStmpset tmp174 (ATSSELboxrec tmp173 0))
  (ATSINStmpset tmp175 (ATSSELboxrec tmp173 1))
  (ATSINStmpset tmp176 (ats2scmpre_list_reverse tmp175))
  (ATSPMVtyrec tmp174 tmp176)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_rtake_while_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret177 #f)
  (tmp185 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_rtake_while_cloref
  (ATSINStmpset tmp185 atscc2scm_null)
  (_ats2scmpre_stream_loop_63 arg1 arg0 0 tmp185)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_loop_63 env0 arg0 arg1 arg2)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
  (tmpret178 #f)
  (tmp179 #f)
  (tmp180 #f)
  (tmp181 #f)
  (tmp182 #f)
  (tmp183 #f)
  (tmp184 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_loop_63
    (ATSINStmpset tmp179 (ATSPMVlazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp179)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         (ATSPMVtyrec arg0 arg2)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp180 (ATSSELcon tmp179 0))
         (ATSINStmpset tmp181 (ATSSELcon tmp179 1))
         (ATSINStmpset tmp182 ((ATSfunclo_fclo env0) env0 arg1 tmp180))
         (if tmp182
           (begin
            (ATSINStmpset tmp183 (ats2scmpre_add_int1_int1 arg1 1))
            (ATSINStmpset tmp184 (ATSPMVtysum tmp180 arg2))
            ;; apy0 = tmp181
            ;; apy1 = tmp183
            ;; apy2 = tmp184
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; arg2 = apy2
            ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_loop_63
            (_ats2scmpre_stream_loop_63 env0 tmp181 tmp183 tmp184)
           ) ;; if-then
           (begin
            (ATSPMVtyrec arg0 arg2)
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret178;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_take_until_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret186 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_take_until_cloref
  (ats2scmpre_stream_take_while_cloref arg0 (_ats2scmpre_stream_patsfun_65__closurerize arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_65 env0 arg0 arg1)
(let(
;;knd = 0
  (tmpret187 #f)
  (tmp188 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_65
  (ATSINStmpset tmp188 ((ATSfunclo_fclo env0) env0 arg0 arg1))
  (ats2scmpre_neg_bool0 tmp188)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_rtake_until_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret189 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_rtake_until_cloref
  (ats2scmpre_stream_rtake_while_cloref arg0 (_ats2scmpre_stream_patsfun_67__closurerize arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_67 env0 arg0 arg1)
(let(
;;knd = 0
  (tmpret190 #f)
  (tmp191 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_67
  (ATSINStmpset tmp191 ((ATSfunclo_fclo env0) env0 arg0 arg1))
  (ats2scmpre_neg_bool0 tmp191)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_list_xprod2 arg0 arg1)
(let(
;;knd = 0
  (tmpret192 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_list_xprod2
  (_ats2scmpre_stream_auxlst_71 arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_aux_69 arg0 arg1)
(let(
;;knd = 0
  (tmpret193 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_aux_69
  (ATSPMVlazyval (_ats2scmpre_stream_patsfun_70__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_70 env0 env1)
(let(
;;knd = 0
  (tmpret194 #f)
  (tmp195 #f)
  (tmp196 #f)
  (tmp197 #f)
  (tmp198 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_70
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons env1)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp195 (ATSSELcon env1 0))
       (ATSINStmpset tmp196 (ATSSELcon env1 1))
       (ATSINStmpset tmp197 (ATSPMVtyrec env0 tmp195))
       (ATSINStmpset tmp198 (_ats2scmpre_stream_aux_69 env0 tmp196))
       (ATSPMVtysum tmp197 tmp198)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_auxlst_71 arg0 arg1)
(let(
;;knd = 0
  (tmpret199 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_auxlst_71
  (ATSPMVlazyval (_ats2scmpre_stream_patsfun_72__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_patsfun_72 env0 env1)
(let(
;;knd = 0
  (tmpret200 #f)
  (tmp201 #f)
  (tmp202 #f)
  (tmp203 #f)
  (tmp204 #f)
  (tmp205 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_patsfun_72
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons env0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp201 (ATSSELcon env0 0))
       (ATSINStmpset tmp202 (ATSSELcon env0 1))
       (ATSINStmpset tmp204 (_ats2scmpre_stream_aux_69 tmp201 env1))
       (ATSINStmpset tmp205 (_ats2scmpre_stream_auxlst_71 tmp202 env1))
       (ATSINStmpset tmp203 (ats2scmpre_stream_append tmp204 tmp205))
       (ATSPMVlazyval_eval tmp203)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun

;;;;;;
;;
;; end-of-compilation-unit
;;
;;;;;;
;;;;;;
;;
;; The Scheme code is generated by atscc2scm
;; The starting compilation time is: 2017-4-12:  0h:35m
;;
;;;;;;

(define
(_ats2scmpre_stream_vt_patsfun_7__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_stream_vt_patsfun_7 (ATSCCget_1 _fcenvs_))) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_10__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_10 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_19__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_19 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_22__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_22 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_25__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_25 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_27__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_27 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_30__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_30 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_32__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_32 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_36__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_36 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_40__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_40 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_44__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_44 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_48__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_48 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_52__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_52 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_stream_vt_patsfun_55__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_stream_vt_patsfun_55 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


;;fun
(define
(ats2scmpre_stream_vt_free arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_free
  (ATSINSmove1_void (atspre_lazy_vt_free arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt2t arg0)
(let(
;;knd = 0
  (tmpret6 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt2t
  (_ats2scmpre_stream_vt_aux_6 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_aux_6 arg0)
(let(
;;knd = 0
  (tmpret7 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_aux_6
  (ATSPMVlazyval (_ats2scmpre_stream_vt_patsfun_7__closurerize arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_7 env0)
(let(
;;knd = 0
  (tmpret8 #f)
  (tmp9 #f)
  (tmp10 #f)
  (tmp11 #f)
  (tmp12 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_7
  (ATSINStmpset tmp9 (ATSPMVllazyval_eval env0))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp9)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp10 (ATSSELcon tmp9 0))
       (ATSINStmpset tmp11 (ATSSELcon tmp9 1))
       ;; ATSINSfreecon(tmp9);
       (ATSINStmpset tmp12 (_ats2scmpre_stream_vt_aux_6 tmp11))
       (ATSPMVtysum tmp10 tmp12)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_takeLte arg0 arg1)
(let(
;;knd = 0
  (tmpret13 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_takeLte
  (_ats2scmpre_stream_vt_auxmain_9 arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_auxmain_9 arg0 arg1)
(let(
;;knd = 0
  (tmpret14 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_auxmain_9
  (ATSPMVllazyval (_ats2scmpre_stream_vt_patsfun_10__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_10 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret15 #f)
  (tmp16 #f)
  (tmp17 #f)
  (tmp18 #f)
  (tmp19 #f)
  (tmp20 #f)
  (tmp21 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_10
  (if arg0
    (begin
     (ATSINStmpset tmp16 (ats2scmpre_gt_int1_int1 env1 0))
     (if tmp16
       (begin
        (ATSINStmpset tmp17 (ATSPMVllazyval_eval env0))
        (letrec(
          (casefnx
           (lambda(tmplab)(case tmplab
            ;; ATSbranchseq_beg
            ((1)
             (if (ATSCKptriscons tmp17)
               (casefnx 4)
               (begin
                (casefnx 2)
               )
             )
            ) ;; end-of-branch
            ((2)
             atscc2scm_null
            ) ;; end-of-branch
            ;; ATSbranchseq_end
            ;; ATSbranchseq_beg
            ((3)
             (casefnx 4)
            ) ;; end-of-branch
            ((4)
             (ATSINStmpset tmp18 (ATSSELcon tmp17 0))
             (ATSINStmpset tmp19 (ATSSELcon tmp17 1))
             ;; ATSINSfreecon(tmp17);
             (ATSINStmpset tmp21 (ats2scmpre_sub_int1_int1 env1 1))
             (ATSINStmpset tmp20 (_ats2scmpre_stream_vt_auxmain_9 tmp19 tmp21))
             (ATSPMVtysum tmp18 tmp20)
            ) ;; end-of-branch
            ;; ATSbranchseq_end
            ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
            ) ;; end-of-case
           ) ;; end-of-lambda
          ) ;; end-of-casefnx
         ) (casefnx 1)
        ) ;; end-of-letrec
       ) ;; if-then
       (begin
        (ATSINSmove1_void (atspre_lazy_vt_free env0))
        atscc2scm_null
       ) ;; if-else
     )
    ) ;; if-then
    (begin
     (ATSINSmove1_void (atspre_lazy_vt_free env0))
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_length arg0)
(let(
;;knd = 0
  (tmpret24 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_length
  (_ats2scmpre_stream_vt_loop_12 arg0 0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_loop_12 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret25 #f)
  (tmp26 #f)
  (tmp28 #f)
  (tmp29 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_vt_loop_12
    (ATSINStmpset tmp26 (ATSPMVllazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp26)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg1
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp28 (ATSSELcon tmp26 1))
         ;; ATSINSfreecon(tmp26);
         (ATSINStmpset tmp29 (ats2scmpre_add_int1_int1 arg1 1))
         ;; apy0 = tmp28
         ;; apy1 = tmp29
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_vt_loop_12
         (_ats2scmpre_stream_vt_loop_12 tmp28 tmp29)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret25;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream2list_vt arg0)
(let(
;;knd = 0
  (tmpret30 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream2list_vt
  (_ats2scmpre_stream_vt_aux_14 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_aux_14 arg0)
(let(
;;knd = 0
  (tmpret31 #f)
  (tmp32 #f)
  (tmp33 #f)
  (tmp34 #f)
  (tmp35 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_aux_14
  (ATSINStmpset tmp32 (ATSPMVllazyval_eval arg0))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp32)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp33 (ATSSELcon tmp32 0))
       (ATSINStmpset tmp34 (ATSSELcon tmp32 1))
       ;; ATSINSfreecon(tmp32);
       (ATSINStmpset tmp35 (_ats2scmpre_stream_vt_aux_14 tmp34))
       (ATSPMVtysum tmp33 tmp35)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream2list_vt_rev arg0)
(let(
;;knd = 0
  (tmpret36 #f)
  (tmp42 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream2list_vt_rev
  (ATSINStmpset tmp42 atscc2scm_null)
  (_ats2scmpre_stream_vt_loop_16 arg0 tmp42)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_loop_16 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret37 #f)
  (tmp38 #f)
  (tmp39 #f)
  (tmp40 #f)
  (tmp41 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_vt_loop_16
    (ATSINStmpset tmp38 (ATSPMVllazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp38)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg1
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp39 (ATSSELcon tmp38 0))
         (ATSINStmpset tmp40 (ATSSELcon tmp38 1))
         ;; ATSINSfreecon(tmp38);
         (ATSINStmpset tmp41 (ATSPMVtysum tmp39 arg1))
         ;; apy0 = tmp40
         ;; apy1 = tmp41
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_vt_loop_16
         (_ats2scmpre_stream_vt_loop_16 tmp40 tmp41)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret37;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_append arg0 arg1)
(let(
;;knd = 0
  (tmpret43 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_append
  (_ats2scmpre_stream_vt_auxmain_18 arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_auxmain_18 arg0 arg1)
(let(
;;knd = 0
  (tmpret44 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_auxmain_18
  (ATSPMVllazyval (_ats2scmpre_stream_vt_patsfun_19__closurerize arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_19 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret45 #f)
  (tmp46 #f)
  (tmp47 #f)
  (tmp48 #f)
  (tmp49 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_19
  (if arg0
    (begin
     (ATSINStmpset tmp46 (ATSPMVllazyval_eval env0))
     (letrec(
       (casefnx
        (lambda(tmplab)(case tmplab
         ;; ATSbranchseq_beg
         ((1)
          (if (ATSCKptriscons tmp46)
            (casefnx 4)
            (begin
             (casefnx 2)
            )
          )
         ) ;; end-of-branch
         ((2)
          (ATSPMVllazyval_eval env1)
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; ATSbranchseq_beg
         ((3)
          (casefnx 4)
         ) ;; end-of-branch
         ((4)
          (ATSINStmpset tmp47 (ATSSELcon tmp46 0))
          (ATSINStmpset tmp48 (ATSSELcon tmp46 1))
          ;; ATSINSfreecon(tmp46);
          (ATSINStmpset tmp49 (_ats2scmpre_stream_vt_auxmain_18 tmp48 env1))
          (ATSPMVtysum tmp47 tmp49)
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
         ) ;; end-of-case
        ) ;; end-of-lambda
       ) ;; end-of-casefnx
      ) (casefnx 1)
     ) ;; end-of-letrec
    ) ;; if-then
    (begin
     (ATSINSmove1_void (atspre_lazy_vt_free env0))
     (ATSINSmove1_void (atspre_lazy_vt_free env1))
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_concat arg0)
(let(
;;knd = 0
  (tmpret52 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_concat
  (_ats2scmpre_stream_vt_auxmain_21 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_auxmain_21 arg0)
(let(
;;knd = 0
  (tmpret53 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_auxmain_21
  (ATSPMVllazyval (_ats2scmpre_stream_vt_patsfun_22__closurerize arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_22 env0 arg0)
(let(
;;knd = 0
  (tmpret54 #f)
  (tmp55 #f)
  (tmp56 #f)
  (tmp57 #f)
  (tmp58 #f)
  (tmp59 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_22
  (if arg0
    (begin
     (ATSINStmpset tmp55 (ATSPMVllazyval_eval env0))
     (letrec(
       (casefnx
        (lambda(tmplab)(case tmplab
         ;; ATSbranchseq_beg
         ((1)
          (if (ATSCKptriscons tmp55)
            (casefnx 4)
            (begin
             (casefnx 2)
            )
          )
         ) ;; end-of-branch
         ((2)
          atscc2scm_null
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; ATSbranchseq_beg
         ((3)
          (casefnx 4)
         ) ;; end-of-branch
         ((4)
          (ATSINStmpset tmp56 (ATSSELcon tmp55 0))
          (ATSINStmpset tmp57 (ATSSELcon tmp55 1))
          ;; ATSINSfreecon(tmp55);
          (ATSINStmpset tmp59 (_ats2scmpre_stream_vt_auxmain_21 tmp57))
          (ATSINStmpset tmp58 (ats2scmpre_stream_vt_append tmp56 tmp59))
          (ATSPMVllazyval_eval tmp58)
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
         ) ;; end-of-case
        ) ;; end-of-lambda
       ) ;; end-of-casefnx
      ) (casefnx 1)
     ) ;; end-of-letrec
    ) ;; if-then
    (begin
     (ATSINSmove1_void (atspre_lazy_vt_free env0))
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_map_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret61 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_map_cloref
  (_ats2scmpre_stream_vt_auxmain_24 arg1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_auxmain_24 env0 arg0)
(let(
;;knd = 0
  (tmpret62 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_auxmain_24
  (ATSPMVllazyval (_ats2scmpre_stream_vt_patsfun_25__closurerize env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_25 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret63 #f)
  (tmp64 #f)
  (tmp65 #f)
  (tmp66 #f)
  (tmp67 #f)
  (tmp68 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_25
  (if arg0
    (begin
     (ATSINStmpset tmp64 (ATSPMVllazyval_eval env1))
     (letrec(
       (casefnx
        (lambda(tmplab)(case tmplab
         ;; ATSbranchseq_beg
         ((1)
          (if (ATSCKptriscons tmp64)
            (casefnx 4)
            (begin
             (casefnx 2)
            )
          )
         ) ;; end-of-branch
         ((2)
          atscc2scm_null
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; ATSbranchseq_beg
         ((3)
          (casefnx 4)
         ) ;; end-of-branch
         ((4)
          (ATSINStmpset tmp65 (ATSSELcon tmp64 0))
          (ATSINStmpset tmp66 (ATSSELcon tmp64 1))
          ;; ATSINSfreecon(tmp64);
          (ATSINStmpset tmp67 ((ATSfunclo_fclo env0) env0 tmp65))
          (ATSINStmpset tmp68 (_ats2scmpre_stream_vt_auxmain_24 env0 tmp66))
          (ATSPMVtysum tmp67 tmp68)
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
         ) ;; end-of-case
        ) ;; end-of-lambda
       ) ;; end-of-casefnx
      ) (casefnx 1)
     ) ;; end-of-letrec
    ) ;; if-then
    (begin
     (ATSINSmove1_void (atspre_lazy_vt_free env1))
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_map_method arg0 arg1)
(let(
;;knd = 0
  (tmpret70 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_map_method
  (_ats2scmpre_stream_vt_patsfun_27__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_27 env0 arg0)
(let(
;;knd = 0
  (tmpret71 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_27
  (ats2scmpre_stream_vt_map_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_filter_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret72 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_filter_cloref
  (_ats2scmpre_stream_vt_auxmain_29 arg1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_auxmain_29 env0 arg0)
(let(
;;knd = 0
  (tmpret73 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_auxmain_29
  (ATSPMVllazyval (_ats2scmpre_stream_vt_patsfun_30__closurerize env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_30 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret74 #f)
  (tmp75 #f)
  (tmp76 #f)
  (tmp77 #f)
  (tmp78 #f)
  (tmp79 #f)
  (tmp80 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_30
  (if arg0
    (begin
     (ATSINStmpset tmp75 (ATSPMVllazyval_eval env1))
     (letrec(
       (casefnx
        (lambda(tmplab)(case tmplab
         ;; ATSbranchseq_beg
         ((1)
          (if (ATSCKptriscons tmp75)
            (casefnx 4)
            (begin
             (casefnx 2)
            )
          )
         ) ;; end-of-branch
         ((2)
          atscc2scm_null
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; ATSbranchseq_beg
         ((3)
          (casefnx 4)
         ) ;; end-of-branch
         ((4)
          (ATSINStmpset tmp76 (ATSSELcon tmp75 0))
          (ATSINStmpset tmp77 (ATSSELcon tmp75 1))
          ;; ATSINSfreecon(tmp75);
          (ATSINStmpset tmp78 ((ATSfunclo_fclo env0) env0 tmp76))
          (if tmp78
            (begin
             (ATSINStmpset tmp79 (_ats2scmpre_stream_vt_auxmain_29 env0 tmp77))
             (ATSPMVtysum tmp76 tmp79)
            ) ;; if-then
            (begin
             (ATSINStmpset tmp80 (_ats2scmpre_stream_vt_auxmain_29 env0 tmp77))
             (ATSPMVllazyval_eval tmp80)
            ) ;; if-else
          )
         ) ;; end-of-branch
         ;; ATSbranchseq_end
         ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
         ) ;; end-of-case
        ) ;; end-of-lambda
       ) ;; end-of-casefnx
      ) (casefnx 1)
     ) ;; end-of-letrec
    ) ;; if-then
    (begin
     (ATSINSmove1_void (atspre_lazy_vt_free env1))
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_filter_method arg0)
(let(
;;knd = 0
  (tmpret82 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_filter_method
  (_ats2scmpre_stream_vt_patsfun_32__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_32 env0 arg0)
(let(
;;knd = 0
  (tmpret83 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_32
  (ats2scmpre_stream_vt_filter_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_exists_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret84 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_exists_cloref
  (_ats2scmpre_stream_vt_loop_34 arg1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_loop_34 env0 arg0)
(let(
;;knd = 1
;;(apy0 #f)
  (tmpret85 #f)
  (tmp86 #f)
  (tmp87 #f)
  (tmp88 #f)
  (tmp89 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_vt_loop_34
    (ATSINStmpset tmp86 (ATSPMVllazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp86)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_false
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp87 (ATSSELcon tmp86 0))
         (ATSINStmpset tmp88 (ATSSELcon tmp86 1))
         ;; ATSINSfreecon(tmp86);
         (ATSINStmpset tmp89 ((ATSfunclo_fclo env0) env0 tmp87))
         (if tmp89
           (begin
            (ATSINSmove1_void (atspre_lazy_vt_free tmp88))
            atscc2scm_true
           ) ;; if-then
           (begin
            ;; apy0 = tmp88
            ;; arg0 = apy0
            ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_vt_loop_34
            (_ats2scmpre_stream_vt_loop_34 env0 tmp88)
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret85;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_exists_method arg0)
(let(
;;knd = 0
  (tmpret91 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_exists_method
  (_ats2scmpre_stream_vt_patsfun_36__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_36 env0 arg0)
(let(
;;knd = 0
  (tmpret92 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_36
  (ats2scmpre_stream_vt_exists_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_forall_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret93 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_forall_cloref
  (_ats2scmpre_stream_vt_loop_38 arg1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_loop_38 env0 arg0)
(let(
;;knd = 1
;;(apy0 #f)
  (tmpret94 #f)
  (tmp95 #f)
  (tmp96 #f)
  (tmp97 #f)
  (tmp98 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_vt_loop_38
    (ATSINStmpset tmp95 (ATSPMVllazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp95)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_true
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp96 (ATSSELcon tmp95 0))
         (ATSINStmpset tmp97 (ATSSELcon tmp95 1))
         ;; ATSINSfreecon(tmp95);
         (ATSINStmpset tmp98 ((ATSfunclo_fclo env0) env0 tmp96))
         (if tmp98
           (begin
            ;; apy0 = tmp97
            ;; arg0 = apy0
            ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_vt_loop_38
            (_ats2scmpre_stream_vt_loop_38 env0 tmp97)
           ) ;; if-then
           (begin
            (ATSINSmove1_void (atspre_lazy_vt_free tmp97))
            atscc2scm_false
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret94;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_forall_method arg0)
(let(
;;knd = 0
  (tmpret100 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_forall_method
  (_ats2scmpre_stream_vt_patsfun_40__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_40 env0 arg0)
(let(
;;knd = 0
  (tmpret101 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_40
  (ats2scmpre_stream_vt_forall_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_foreach_cloref arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_foreach_cloref
  (ATSINSmove1_void (_ats2scmpre_stream_vt_loop_42 arg1 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_loop_42 env0 arg0)
(let(
;;knd = 1
;;(apy0 #f)
  (tmp104 #f)
  (tmp105 #f)
  (tmp106 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_vt_loop_42
    (ATSINStmpset tmp104 (ATSPMVllazyval_eval arg0))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp104)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         ATSINSmove0_void
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp105 (ATSSELcon tmp104 0))
         (ATSINStmpset tmp106 (ATSSELcon tmp104 1))
         ;; ATSINSfreecon(tmp104);
         (ATSINSmove1_void ((ATSfunclo_fclo env0) env0 tmp105))
         ;; apy0 = tmp106
         ;; arg0 = apy0
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_vt_loop_42
         (_ats2scmpre_stream_vt_loop_42 env0 tmp106)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return/*_void*/;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_foreach_method arg0)
(let(
;;knd = 0
  (tmpret108 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_foreach_method
  (_ats2scmpre_stream_vt_patsfun_44__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_44 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_44
  (ATSINSmove1_void (ats2scmpre_stream_vt_foreach_cloref env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_iforeach_cloref arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_iforeach_cloref
  (ATSINSmove1_void (_ats2scmpre_stream_vt_loop_46 arg1 0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_loop_46 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmp112 #f)
  (tmp113 #f)
  (tmp114 #f)
  (tmp116 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_stream_vt_loop_46
    (ATSINStmpset tmp112 (ATSPMVllazyval_eval arg1))
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons tmp112)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         ATSINSmove0_void
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp113 (ATSSELcon tmp112 0))
         (ATSINStmpset tmp114 (ATSSELcon tmp112 1))
         ;; ATSINSfreecon(tmp112);
         (ATSINSmove1_void ((ATSfunclo_fclo env0) env0 arg0 tmp113))
         (ATSINStmpset tmp116 (ats2scmpre_add_int1_int1 arg0 1))
         ;; apy0 = tmp116
         ;; apy1 = tmp114
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_stream_vt_loop_46
         (_ats2scmpre_stream_vt_loop_46 env0 tmp116 tmp114)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return/*_void*/;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_iforeach_method arg0)
(let(
;;knd = 0
  (tmpret117 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_iforeach_method
  (_ats2scmpre_stream_vt_patsfun_48__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_48 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_48
  (ATSINSmove1_void (ats2scmpre_stream_vt_iforeach_cloref env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_rforeach_cloref arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_rforeach_cloref
  (ATSINSmove1_void (_ats2scmpre_stream_vt_auxmain_50 arg1 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_auxmain_50 env0 arg0)
(let(
;;knd = 0
  (tmp121 #f)
  (tmp122 #f)
  (tmp123 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_auxmain_50
  (ATSINStmpset tmp121 (ATSPMVllazyval_eval arg0))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp121)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       ATSINSmove0_void
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp122 (ATSSELcon tmp121 0))
       (ATSINStmpset tmp123 (ATSSELcon tmp121 1))
       ;; ATSINSfreecon(tmp121);
       (ATSINSmove1_void (_ats2scmpre_stream_vt_auxmain_50 env0 tmp123))
       (ATSINSmove1_void ((ATSfunclo_fclo env0) env0 tmp122))
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_rforeach_method arg0)
(let(
;;knd = 0
  (tmpret125 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_rforeach_method
  (_ats2scmpre_stream_vt_patsfun_52__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_52 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_52
  (ATSINSmove1_void (ats2scmpre_stream_vt_rforeach_cloref env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_stream_vt_tabulate_cloref arg0)
(let(
;;knd = 0
  (tmpret127 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_stream_vt_tabulate_cloref
  (_ats2scmpre_stream_vt_auxmain_54 arg0 0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_auxmain_54 env0 arg0)
(let(
;;knd = 0
  (tmpret128 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_auxmain_54
  (ATSPMVllazyval (_ats2scmpre_stream_vt_patsfun_55__closurerize env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_stream_vt_patsfun_55 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret129 #f)
  (tmp130 #f)
  (tmp131 #f)
  (tmp132 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_stream_vt_patsfun_55
  (if arg0
    (begin
     (ATSINStmpset tmp130 ((ATSfunclo_fclo env0) env0 env1))
     (ATSINStmpset tmp132 (ats2scmpre_add_int1_int1 env1 1))
     (ATSINStmpset tmp131 (_ats2scmpre_stream_vt_auxmain_54 env0 tmp132))
     (ATSPMVtysum tmp130 tmp131)
    ) ;; if-then
    (begin
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun

;;;;;;
;;
;; end-of-compilation-unit
;;
;;;;;;
;;;;;;
;;
;; The Scheme code is generated by atscc2scm
;; The starting compilation time is: 2017-4-12:  0h:35m
;;
;;;;;;

(define
(_ats2scmpre_intrange_patsfun_4__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_4 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_9__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_9 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_11__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_11 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_13__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_13 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_16__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_16 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_20__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_20 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_23__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_23 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_26__closurerize xenv0 xenv1 xenv2)
;;%{
  (list (lambda(_fcenvs_)(_ats2scmpre_intrange_patsfun_26 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) (ATSCCget_3 _fcenvs_))) xenv0 xenv1 xenv2)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_28__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_28 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_31__closurerize xenv0 xenv1 xenv2)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_31 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) (ATSCCget_3 _fcenvs_) xarg0)) xenv0 xenv1 xenv2)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_33__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_33 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_40__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_40 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_44__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_44 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_48__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_48 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_intrange_patsfun_52__closurerize xenv0 xenv1 xenv2)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_intrange_patsfun_52 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) (ATSCCget_3 _fcenvs_) xarg0)) xenv0 xenv1 xenv2)
;;%}
) ;; define


;;fun
(define
(ats2scmpre_int_repeat_lazy arg0 arg1)
(let(
;;knd = 0
  (tmp1 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_repeat_lazy
  (ATSINStmpset tmp1 (ats2scmpre_lazy2cloref arg1))
  (ATSINSmove1_void (ats2scmpre_int_repeat_cloref arg0 tmp1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_repeat_cloref arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_repeat_cloref
  (ATSINSmove1_void (_ats2scmpre_intrange_loop_2 arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_loop_2 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmp4 #f)
  (tmp6 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_intrange_loop_2
    (ATSINStmpset tmp4 (ats2scmpre_gt_int0_int0 arg0 0))
    (if tmp4
      (begin
       (ATSINSmove1_void ((ATSfunclo_fclo arg1) arg1))
       (ATSINStmpset tmp6 (ats2scmpre_sub_int0_int0 arg0 1))
       ;; apy0 = tmp6
       ;; apy1 = arg1
       ;; arg0 = apy0
       ;; arg1 = apy1
       ;; funlab_scm = 1; // __patsflab__ats2scmpre_intrange_loop_2
       (_ats2scmpre_intrange_loop_2 tmp6 arg1)
      ) ;; if-then
      (begin
       ATSINSmove0_void
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return/*_void*/;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_repeat_method arg0)
(let(
;;knd = 0
  (tmpret7 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_repeat_method
  (_ats2scmpre_intrange_patsfun_4__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_4 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_4
  (ATSINSmove1_void (ats2scmpre_int_repeat_cloref env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_exists_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret9 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_exists_cloref
  (ats2scmpre_intrange_exists_cloref 0 arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_forall_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret10 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_forall_cloref
  (ats2scmpre_intrange_forall_cloref 0 arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_foreach_cloref arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_foreach_cloref
  (ATSINSmove1_void (ats2scmpre_intrange_foreach_cloref 0 arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_exists_method arg0)
(let(
;;knd = 0
  (tmpret12 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_exists_method
  (_ats2scmpre_intrange_patsfun_9__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_9 env0 arg0)
(let(
;;knd = 0
  (tmpret13 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_9
  (ats2scmpre_int_exists_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_forall_method arg0)
(let(
;;knd = 0
  (tmpret14 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_forall_method
  (_ats2scmpre_intrange_patsfun_11__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_11 env0 arg0)
(let(
;;knd = 0
  (tmpret15 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_11
  (ats2scmpre_int_forall_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_foreach_method arg0)
(let(
;;knd = 0
  (tmpret16 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_foreach_method
  (_ats2scmpre_intrange_patsfun_13__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_13 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_13
  (ATSINSmove1_void (ats2scmpre_int_foreach_cloref env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_foldleft_cloref arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret18 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_foldleft_cloref
  (ats2scmpre_intrange_foldleft_cloref 0 arg0 arg1 arg2)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_foldleft_method arg0 arg1)
(let(
;;knd = 0
  (tmpret19 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_foldleft_method
  (_ats2scmpre_intrange_patsfun_16__closurerize arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_16 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret20 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_16
  (ats2scmpre_int_foldleft_cloref env0 env1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_list_map_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret21 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_list_map_cloref
  (_ats2scmpre_intrange_aux_18 arg0 arg1 0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_aux_18 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret22 #f)
  (tmp23 #f)
  (tmp24 #f)
  (tmp25 #f)
  (tmp26 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_aux_18
  (ATSINStmpset tmp23 (ats2scmpre_lt_int1_int1 arg0 env0))
  (if tmp23
    (begin
     (ATSINStmpset tmp24 ((ATSfunclo_fclo env1) env1 arg0))
     (ATSINStmpset tmp26 (ats2scmpre_add_int1_int1 arg0 1))
     (ATSINStmpset tmp25 (_ats2scmpre_intrange_aux_18 env0 env1 tmp26))
     (ATSPMVtysum tmp24 tmp25)
    ) ;; if-then
    (begin
     atscc2scm_null
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_list_map_method arg0 arg1)
(let(
;;knd = 0
  (tmpret27 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_list_map_method
  (_ats2scmpre_intrange_patsfun_20__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_20 env0 arg0)
(let(
;;knd = 0
  (tmpret28 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_20
  (ats2scmpre_int_list_map_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_list0_map_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret29 #f)
  (tmp30 #f)
  (tmp31 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_list0_map_cloref
  (ATSINStmpset tmp30 (ats2scmpre_gte_int1_int1 arg0 0))
  (if tmp30
    (begin
     (ATSINStmpset tmp31 (ats2scmpre_int_list_map_cloref arg0 arg1))
     tmp31
    ) ;; if-then
    (begin
     atscc2scm_null
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_list0_map_method arg0 arg1)
(let(
;;knd = 0
  (tmpret32 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_list0_map_method
  (_ats2scmpre_intrange_patsfun_23__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_23 env0 arg0)
(let(
;;knd = 0
  (tmpret33 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_23
  (ats2scmpre_int_list0_map_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_stream_map_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret34 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_stream_map_cloref
  (_ats2scmpre_intrange_aux_25 arg0 arg1 0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_aux_25 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret35 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_aux_25
  (ATSPMVlazyval (_ats2scmpre_intrange_patsfun_26__closurerize env0 env1 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_26 env0 env1 env2)
(let(
;;knd = 0
  (tmpret36 #f)
  (tmp37 #f)
  (tmp38 #f)
  (tmp39 #f)
  (tmp40 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_26
  (ATSINStmpset tmp37 (ats2scmpre_lt_int1_int1 env2 env0))
  (if tmp37
    (begin
     (ATSINStmpset tmp38 ((ATSfunclo_fclo env1) env1 env2))
     (ATSINStmpset tmp40 (ats2scmpre_add_int1_int1 env2 1))
     (ATSINStmpset tmp39 (_ats2scmpre_intrange_aux_25 env0 env1 tmp40))
     (ATSPMVtysum tmp38 tmp39)
    ) ;; if-then
    (begin
     atscc2scm_null
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_stream_map_method arg0 arg1)
(let(
;;knd = 0
  (tmpret41 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_stream_map_method
  (_ats2scmpre_intrange_patsfun_28__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_28 env0 arg0)
(let(
;;knd = 0
  (tmpret42 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_28
  (ats2scmpre_int_stream_map_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_stream_vt_map_cloref arg0 arg1)
(let(
;;knd = 0
  (tmpret43 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_stream_vt_map_cloref
  (_ats2scmpre_intrange_aux_30 arg0 arg1 0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_aux_30 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret44 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_aux_30
  (ATSPMVllazyval (_ats2scmpre_intrange_patsfun_31__closurerize env0 env1 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_31 env0 env1 env2 arg0)
(let(
;;knd = 0
  (tmpret45 #f)
  (tmp46 #f)
  (tmp47 #f)
  (tmp48 #f)
  (tmp49 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_31
  (if arg0
    (begin
     (ATSINStmpset tmp46 (ats2scmpre_lt_int1_int1 env2 env0))
     (if tmp46
       (begin
        (ATSINStmpset tmp47 ((ATSfunclo_fclo env1) env1 env2))
        (ATSINStmpset tmp49 (ats2scmpre_add_int1_int1 env2 1))
        (ATSINStmpset tmp48 (_ats2scmpre_intrange_aux_30 env0 env1 tmp49))
        (ATSPMVtysum tmp47 tmp48)
       ) ;; if-then
       (begin
        atscc2scm_null
       ) ;; if-else
     )
    ) ;; if-then
    (begin
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int_stream_vt_map_method arg0 arg1)
(let(
;;knd = 0
  (tmpret50 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int_stream_vt_map_method
  (_ats2scmpre_intrange_patsfun_33__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_33 env0 arg0)
(let(
;;knd = 0
  (tmpret51 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_33
  (ats2scmpre_int_stream_vt_map_cloref env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int2_exists_cloref arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret52 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int2_exists_cloref
  (ats2scmpre_intrange2_exists_cloref 0 arg0 0 arg1 arg2)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int2_forall_cloref arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret53 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int2_forall_cloref
  (ats2scmpre_intrange2_forall_cloref 0 arg0 0 arg1 arg2)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_int2_foreach_cloref arg0 arg1 arg2)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_int2_foreach_cloref
  (ATSINSmove1_void (ats2scmpre_intrange2_foreach_cloref 0 arg0 0 arg1 arg2))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange_exists_cloref arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret55 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange_exists_cloref
  (_ats2scmpre_intrange_loop_38 arg0 arg1 arg2)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_loop_38 arg0 arg1 arg2)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
  (tmpret56 #f)
  (tmp57 #f)
  (tmp58 #f)
  (tmp59 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_intrange_loop_38
    (ATSINStmpset tmp57 (ats2scmpre_lt_int0_int0 arg0 arg1))
    (if tmp57
      (begin
       (ATSINStmpset tmp58 ((ATSfunclo_fclo arg2) arg2 arg0))
       (if tmp58
         (begin
          atscc2scm_true
         ) ;; if-then
         (begin
          (ATSINStmpset tmp59 (ats2scmpre_add_int0_int0 arg0 1))
          ;; apy0 = tmp59
          ;; apy1 = arg1
          ;; apy2 = arg2
          ;; arg0 = apy0
          ;; arg1 = apy1
          ;; arg2 = apy2
          ;; funlab_scm = 1; // __patsflab__ats2scmpre_intrange_loop_38
          (_ats2scmpre_intrange_loop_38 tmp59 arg1 arg2)
         ) ;; if-else
       )
      ) ;; if-then
      (begin
       atscc2scm_false
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret56;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange_exists_method arg0)
(let(
;;knd = 0
  (tmpret60 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange_exists_method
  (_ats2scmpre_intrange_patsfun_40__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_40 env0 arg0)
(let(
;;knd = 0
  (tmpret61 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_40
  (ats2scmpre_intrange_exists_cloref (ATSSELboxrec env0 0) (ATSSELboxrec env0 1) arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange_forall_cloref arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret62 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange_forall_cloref
  (_ats2scmpre_intrange_loop_42 arg0 arg1 arg2)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_loop_42 arg0 arg1 arg2)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
  (tmpret63 #f)
  (tmp64 #f)
  (tmp65 #f)
  (tmp66 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_intrange_loop_42
    (ATSINStmpset tmp64 (ats2scmpre_lt_int0_int0 arg0 arg1))
    (if tmp64
      (begin
       (ATSINStmpset tmp65 ((ATSfunclo_fclo arg2) arg2 arg0))
       (if tmp65
         (begin
          (ATSINStmpset tmp66 (ats2scmpre_add_int0_int0 arg0 1))
          ;; apy0 = tmp66
          ;; apy1 = arg1
          ;; apy2 = arg2
          ;; arg0 = apy0
          ;; arg1 = apy1
          ;; arg2 = apy2
          ;; funlab_scm = 1; // __patsflab__ats2scmpre_intrange_loop_42
          (_ats2scmpre_intrange_loop_42 tmp66 arg1 arg2)
         ) ;; if-then
         (begin
          atscc2scm_false
         ) ;; if-else
       )
      ) ;; if-then
      (begin
       atscc2scm_true
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret63;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange_forall_method arg0)
(let(
;;knd = 0
  (tmpret67 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange_forall_method
  (_ats2scmpre_intrange_patsfun_44__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_44 env0 arg0)
(let(
;;knd = 0
  (tmpret68 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_44
  (ats2scmpre_intrange_forall_cloref (ATSSELboxrec env0 0) (ATSSELboxrec env0 1) arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange_foreach_cloref arg0 arg1 arg2)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange_foreach_cloref
  (ATSINSmove1_void (_ats2scmpre_intrange_loop_46 arg0 arg1 arg2))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_loop_46 arg0 arg1 arg2)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
  (tmp71 #f)
  (tmp73 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_intrange_loop_46
    (ATSINStmpset tmp71 (ats2scmpre_lt_int0_int0 arg0 arg1))
    (if tmp71
      (begin
       (ATSINSmove1_void ((ATSfunclo_fclo arg2) arg2 arg0))
       (ATSINStmpset tmp73 (ats2scmpre_add_int0_int0 arg0 1))
       ;; apy0 = tmp73
       ;; apy1 = arg1
       ;; apy2 = arg2
       ;; arg0 = apy0
       ;; arg1 = apy1
       ;; arg2 = apy2
       ;; funlab_scm = 1; // __patsflab__ats2scmpre_intrange_loop_46
       (_ats2scmpre_intrange_loop_46 tmp73 arg1 arg2)
      ) ;; if-then
      (begin
       ATSINSmove0_void
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return/*_void*/;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange_foreach_method arg0)
(let(
;;knd = 0
  (tmpret74 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange_foreach_method
  (_ats2scmpre_intrange_patsfun_48__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_48 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_48
  (ATSINSmove1_void (ats2scmpre_intrange_foreach_cloref (ATSSELboxrec env0 0) (ATSSELboxrec env0 1) arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange_foldleft_cloref arg0 arg1 arg2 arg3)
(let(
;;knd = 0
  (tmpret76 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange_foldleft_cloref
  (_ats2scmpre_intrange_loop_50 arg3 arg0 arg1 arg2 arg3)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_loop_50 env0 arg0 arg1 arg2 arg3)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
;;(apy3 #f)
  (tmpret77 #f)
  (tmp78 #f)
  (tmp79 #f)
  (tmp80 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_intrange_loop_50
    (ATSINStmpset tmp78 (ats2scmpre_lt_int0_int0 arg0 arg1))
    (if tmp78
      (begin
       (ATSINStmpset tmp79 (ats2scmpre_add_int0_int0 arg0 1))
       (ATSINStmpset tmp80 ((ATSfunclo_fclo arg3) arg3 arg2 arg0))
       ;; apy0 = tmp79
       ;; apy1 = arg1
       ;; apy2 = tmp80
       ;; apy3 = env0
       ;; arg0 = apy0
       ;; arg1 = apy1
       ;; arg2 = apy2
       ;; arg3 = apy3
       ;; funlab_scm = 1; // __patsflab__ats2scmpre_intrange_loop_50
       (_ats2scmpre_intrange_loop_50 env0 tmp79 arg1 tmp80 env0)
      ) ;; if-then
      (begin
       arg2
      ) ;; if-else
    )
    ;; if (funlab_scm > 0) continue; else ;; return tmpret77;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange_foldleft_method arg0 arg1)
(let(
;;knd = 0
  (tmp81 #f)
  (tmp82 #f)
  (tmpret83 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange_foldleft_method
  (ATSINStmpset tmp81 (ATSSELboxrec arg0 0))
  (ATSINStmpset tmp82 (ATSSELboxrec arg0 1))
  (_ats2scmpre_intrange_patsfun_52__closurerize tmp81 tmp82 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_patsfun_52 env0 env1 env2 arg0)
(let(
;;knd = 0
  (tmpret84 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_intrange_patsfun_52
  (ats2scmpre_intrange_foldleft_cloref env0 env1 env2 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange2_exists_cloref arg0 arg1 arg2 arg3 arg4)
(let(
;;knd = 0
  (tmpret85 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange2_exists_cloref
  (_ats2scmpre_intrange_loop1_54 arg2 arg3 arg4 arg0 arg1 arg2 arg3 arg4)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_loop1_54 env0 env1 env2 arg0 arg1 arg2 arg3 arg4)
(_ats2scmpre_intrange_loop1_54__ 1 env0 env1 env2 arg0 arg1 arg2 arg3 arg4)
) ;; end-of-fun
(define
(_ats2scmpre_intrange_loop1_54__ funlab env0 env1 env2 arg0 arg1 arg2 arg3 arg4)
(let(
;;knd = 2
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
;;(apy3 #f)
;;(apy4 #f)
  (tmpret86 #f)
  (tmp87 #f)
  (a2rg0 arg0)
  (a2rg1 arg1)
  (a2rg2 arg2)
  (a2rg3 arg3)
  (a2rg4 arg4)
;;(a2py0 #f)
;;(a2py1 #f)
;;(a2py2 #f)
;;(a2py3 #f)
;;(a2py4 #f)
  (tmpret88 #f)
  (tmp89 #f)
  (tmp90 #f)
  (tmp91 #f)
  (tmp92 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; funlab_scm = 1;
  ;while(true) {
    ;switch(funlab_scm) {
    (case funlab
      ;case 1: {
      ((1)
        ;; funlab_scm = 0;
        (ATSINStmpset tmp87 (ats2scmpre_lt_int0_int0 arg0 arg1))
        (if tmp87
          (begin
           ;; a2py0 = arg0
           ;; a2py1 = arg1
           ;; a2py2 = arg2
           ;; a2py3 = arg3
           ;; a2py4 = env2
           ;; a2rg0 = a2py0
           ;; a2rg1 = a2py1
           ;; a2rg2 = a2py2
           ;; a2rg3 = a2py3
           ;; a2rg4 = a2py4
           ;; funlab_scm = 2; // __patsflab__ats2scmpre_intrange_loop2_55
           (_ats2scmpre_intrange_loop1_54__ 2 env0 env1 env2 arg0 arg1 arg2 arg3 env2)
          ) ;; if-then
          (begin
           atscc2scm_false
          ) ;; if-else
        );
        ;; if (funlab_scm > 0) continue; else ;; return tmpret86;
      ) ;} // end-of-case
      ;case 2: {
      ((2)
        ;; funlab_scm = 0;
        (ATSINStmpset tmp89 (ats2scmpre_lt_int0_int0 a2rg2 a2rg3))
        (if tmp89
          (begin
           (ATSINStmpset tmp90 ((ATSfunclo_fclo a2rg4) a2rg4 a2rg0 a2rg2))
           (if tmp90
             (begin
              atscc2scm_true
             ) ;; if-then
             (begin
              (ATSINStmpset tmp91 (ats2scmpre_add_int0_int0 a2rg2 1))
              ;; a2py0 = a2rg0
              ;; a2py1 = a2rg1
              ;; a2py2 = tmp91
              ;; a2py3 = a2rg3
              ;; a2py4 = a2rg4
              ;; a2rg0 = a2py0
              ;; a2rg1 = a2py1
              ;; a2rg2 = a2py2
              ;; a2rg3 = a2py3
              ;; a2rg4 = a2py4
              ;; funlab_scm = 2; // __patsflab__ats2scmpre_intrange_loop2_55
              (_ats2scmpre_intrange_loop1_54__ 2 env0 env1 env2 a2rg0 a2rg1 tmp91 a2rg3 a2rg4)
             ) ;; if-else
           )
          ) ;; if-then
          (begin
           (ATSINStmpset tmp92 (ats2scmpre_add_int0_int0 a2rg0 1))
           ;; apy0 = tmp92
           ;; apy1 = a2rg1
           ;; apy2 = env0
           ;; apy3 = env1
           ;; apy4 = a2rg4
           ;; arg0 = apy0
           ;; arg1 = apy1
           ;; arg2 = apy2
           ;; arg3 = apy3
           ;; arg4 = apy4
           ;; funlab_scm = 1; // __patsflab__ats2scmpre_intrange_loop1_54
           (_ats2scmpre_intrange_loop1_54__ 1 env0 env1 env2 tmp92 a2rg1 env0 env1 a2rg4)
          ) ;; if-else
        );
        ;; if (funlab_scm > 0) continue; else ;; return tmpret88;
      ) ;} // end-of-case
    ) ;} // end-of-switch
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange2_forall_cloref arg0 arg1 arg2 arg3 arg4)
(let(
;;knd = 0
  (tmpret93 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange2_forall_cloref
  (_ats2scmpre_intrange_loop1_57 arg2 arg3 arg0 arg1 arg2 arg3 arg4)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_loop1_57 env0 env1 arg0 arg1 arg2 arg3 arg4)
(_ats2scmpre_intrange_loop1_57__ 1 env0 env1 arg0 arg1 arg2 arg3 arg4)
) ;; end-of-fun
(define
(_ats2scmpre_intrange_loop1_57__ funlab env0 env1 arg0 arg1 arg2 arg3 arg4)
(let(
;;knd = 2
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
;;(apy3 #f)
;;(apy4 #f)
  (tmpret94 #f)
  (tmp95 #f)
  (a2rg0 arg0)
  (a2rg1 arg1)
  (a2rg2 arg2)
  (a2rg3 arg3)
  (a2rg4 arg4)
;;(a2py0 #f)
;;(a2py1 #f)
;;(a2py2 #f)
;;(a2py3 #f)
;;(a2py4 #f)
  (tmpret96 #f)
  (tmp97 #f)
  (tmp98 #f)
  (tmp99 #f)
  (tmp100 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; funlab_scm = 1;
  ;while(true) {
    ;switch(funlab_scm) {
    (case funlab
      ;case 1: {
      ((1)
        ;; funlab_scm = 0;
        (ATSINStmpset tmp95 (ats2scmpre_lt_int0_int0 arg0 arg1))
        (if tmp95
          (begin
           ;; a2py0 = arg0
           ;; a2py1 = arg1
           ;; a2py2 = arg2
           ;; a2py3 = arg3
           ;; a2py4 = arg4
           ;; a2rg0 = a2py0
           ;; a2rg1 = a2py1
           ;; a2rg2 = a2py2
           ;; a2rg3 = a2py3
           ;; a2rg4 = a2py4
           ;; funlab_scm = 2; // __patsflab__ats2scmpre_intrange_loop2_58
           (_ats2scmpre_intrange_loop1_57__ 2 env0 env1 arg0 arg1 arg2 arg3 arg4)
          ) ;; if-then
          (begin
           atscc2scm_true
          ) ;; if-else
        );
        ;; if (funlab_scm > 0) continue; else ;; return tmpret94;
      ) ;} // end-of-case
      ;case 2: {
      ((2)
        ;; funlab_scm = 0;
        (ATSINStmpset tmp97 (ats2scmpre_lt_int0_int0 a2rg2 a2rg3))
        (if tmp97
          (begin
           (ATSINStmpset tmp98 ((ATSfunclo_fclo a2rg4) a2rg4 a2rg0 a2rg2))
           (if tmp98
             (begin
              (ATSINStmpset tmp99 (ats2scmpre_add_int0_int0 a2rg2 1))
              ;; a2py0 = a2rg0
              ;; a2py1 = a2rg1
              ;; a2py2 = tmp99
              ;; a2py3 = a2rg3
              ;; a2py4 = a2rg4
              ;; a2rg0 = a2py0
              ;; a2rg1 = a2py1
              ;; a2rg2 = a2py2
              ;; a2rg3 = a2py3
              ;; a2rg4 = a2py4
              ;; funlab_scm = 2; // __patsflab__ats2scmpre_intrange_loop2_58
              (_ats2scmpre_intrange_loop1_57__ 2 env0 env1 a2rg0 a2rg1 tmp99 a2rg3 a2rg4)
             ) ;; if-then
             (begin
              atscc2scm_false
             ) ;; if-else
           )
          ) ;; if-then
          (begin
           (ATSINStmpset tmp100 (ats2scmpre_add_int0_int0 a2rg0 1))
           ;; apy0 = tmp100
           ;; apy1 = a2rg1
           ;; apy2 = env0
           ;; apy3 = env1
           ;; apy4 = a2rg4
           ;; arg0 = apy0
           ;; arg1 = apy1
           ;; arg2 = apy2
           ;; arg3 = apy3
           ;; arg4 = apy4
           ;; funlab_scm = 1; // __patsflab__ats2scmpre_intrange_loop1_57
           (_ats2scmpre_intrange_loop1_57__ 1 env0 env1 tmp100 a2rg1 env0 env1 a2rg4)
          ) ;; if-else
        );
        ;; if (funlab_scm > 0) continue; else ;; return tmpret96;
      ) ;} // end-of-case
    ) ;} // end-of-switch
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_intrange2_foreach_cloref arg0 arg1 arg2 arg3 arg4)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_intrange2_foreach_cloref
  (ATSINSmove1_void (_ats2scmpre_intrange_loop1_60 arg2 arg3 arg0 arg1 arg2 arg3 arg4))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_intrange_loop1_60 env0 env1 arg0 arg1 arg2 arg3 arg4)
(_ats2scmpre_intrange_loop1_60__ 1 env0 env1 arg0 arg1 arg2 arg3 arg4)
) ;; end-of-fun
(define
(_ats2scmpre_intrange_loop1_60__ funlab env0 env1 arg0 arg1 arg2 arg3 arg4)
(let(
;;knd = 2
;;(apy0 #f)
;;(apy1 #f)
;;(apy2 #f)
;;(apy3 #f)
;;(apy4 #f)
  (tmp103 #f)
  (a2rg0 arg0)
  (a2rg1 arg1)
  (a2rg2 arg2)
  (a2rg3 arg3)
  (a2rg4 arg4)
;;(a2py0 #f)
;;(a2py1 #f)
;;(a2py2 #f)
;;(a2py3 #f)
;;(a2py4 #f)
  (tmp105 #f)
  (tmp107 #f)
  (tmp108 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; funlab_scm = 1;
  ;while(true) {
    ;switch(funlab_scm) {
    (case funlab
      ;case 1: {
      ((1)
        ;; funlab_scm = 0;
        (ATSINStmpset tmp103 (ats2scmpre_lt_int0_int0 arg0 arg1))
        (if tmp103
          (begin
           ;; a2py0 = arg0
           ;; a2py1 = arg1
           ;; a2py2 = arg2
           ;; a2py3 = arg3
           ;; a2py4 = arg4
           ;; a2rg0 = a2py0
           ;; a2rg1 = a2py1
           ;; a2rg2 = a2py2
           ;; a2rg3 = a2py3
           ;; a2rg4 = a2py4
           ;; funlab_scm = 2; // __patsflab__ats2scmpre_intrange_loop2_61
           (_ats2scmpre_intrange_loop1_60__ 2 env0 env1 arg0 arg1 arg2 arg3 arg4)
          ) ;; if-then
          (begin
           ATSINSmove0_void
          ) ;; if-else
        );
        ;; if (funlab_scm > 0) continue; else ;; return/*_void*/;
      ) ;} // end-of-case
      ;case 2: {
      ((2)
        ;; funlab_scm = 0;
        (ATSINStmpset tmp105 (ats2scmpre_lt_int0_int0 a2rg2 a2rg3))
        (if tmp105
          (begin
           (ATSINSmove1_void ((ATSfunclo_fclo a2rg4) a2rg4 a2rg0 a2rg2))
           (ATSINStmpset tmp107 (ats2scmpre_add_int0_int0 a2rg2 1))
           ;; a2py0 = a2rg0
           ;; a2py1 = a2rg1
           ;; a2py2 = tmp107
           ;; a2py3 = a2rg3
           ;; a2py4 = a2rg4
           ;; a2rg0 = a2py0
           ;; a2rg1 = a2py1
           ;; a2rg2 = a2py2
           ;; a2rg3 = a2py3
           ;; a2rg4 = a2py4
           ;; funlab_scm = 2; // __patsflab__ats2scmpre_intrange_loop2_61
           (_ats2scmpre_intrange_loop1_60__ 2 env0 env1 a2rg0 a2rg1 tmp107 a2rg3 a2rg4)
          ) ;; if-then
          (begin
           (ATSINStmpset tmp108 (ats2scmpre_succ_int0 a2rg0))
           ;; apy0 = tmp108
           ;; apy1 = a2rg1
           ;; apy2 = env0
           ;; apy3 = env1
           ;; apy4 = a2rg4
           ;; arg0 = apy0
           ;; arg1 = apy1
           ;; arg2 = apy2
           ;; arg3 = apy3
           ;; arg4 = apy4
           ;; funlab_scm = 1; // __patsflab__ats2scmpre_intrange_loop1_60
           (_ats2scmpre_intrange_loop1_60__ 1 env0 env1 tmp108 a2rg1 env0 env1 a2rg4)
          ) ;; if-else
        );
        ;; if (funlab_scm > 0) continue; else ;; return/*_void*/;
      ) ;} // end-of-case
    ) ;} // end-of-switch
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun

;;;;;;
;;
;; end-of-compilation-unit
;;
;;;;;;
;;;;;;
;;
;; The Scheme code is generated by atscc2scm
;; The starting compilation time is: 2017-4-12:  0h:35m
;;
;;;;;;
;;;;;;
;;
;; end-of-compilation-unit
;;
;;;;;;
;;;;;;
;;
;; The Scheme code is generated by atscc2scm
;; The starting compilation time is: 2017-4-12:  0h:35m
;;
;;;;;;

;;fun
(define
(slistref_make_nil)
(let(
;;knd = 0
  (tmpret0 #f)
  (tmp1 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_slistref_make_nil
  (ATSINStmpset tmp1 atscc2scm_null)
  (ats2scmpre_ref tmp1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(slistref_length arg0)
(let(
;;knd = 0
  (tmpret2 #f)
  (tmp3 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_slistref_length
  (ATSINStmpset tmp3 (ats2scmpre_ref_get_elt arg0))
  (ats2scmpre_list_length tmp3)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(slistref_push arg0 arg1)
(let(
;;knd = 0
  (tmp5 #f)
  (tmp6 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_slistref_push
  (ATSINStmpset tmp6 (ats2scmpre_ref_get_elt arg0))
  (ATSINStmpset tmp5 (ATSPMVtysum arg1 tmp6))
  (ATSINSmove1_void (ats2scmpre_ref_set_elt arg0 tmp5))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(slistref_pop_opt arg0)
(let(
;;knd = 0
  (tmpret7 #f)
  (tmp8 #f)
  (tmp9 #f)
  (tmp10 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_slistref_pop_opt
  (ATSINStmpset tmp8 (ats2scmpre_ref_get_elt arg0))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp8)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp9 (ATSSELcon tmp8 0))
       (ATSINStmpset tmp10 (ATSSELcon tmp8 1))
       (ATSINSmove1_void (ats2scmpre_ref_set_elt arg0 tmp10))
       (ATSPMVtysum tmp9)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(slistref_foldleft arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret12 #f)
  (tmp13 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_slistref_foldleft
  (ATSINStmpset tmp13 (ats2scmpre_ref_get_elt arg0))
  (ats2scmpre_list_foldleft tmp13 arg1 arg2)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(slistref_foldright arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret14 #f)
  (tmp15 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_slistref_foldright
  (ATSINStmpset tmp15 (ats2scmpre_ref_get_elt arg0))
  (ats2scmpre_list_foldright tmp15 arg1 arg2)
) ;; end-of-let
) ;; end-of-fun

;;;;;;
;;
;; end-of-compilation-unit
;;
;;;;;;
;;;;;;
;;
;; The Scheme code is generated by atscc2scm
;; The starting compilation time is: 2017-4-12:  0h:35m
;;
;;;;;;

;;fun
(define
(ats2scmpre_qlistref_make_nil)
(let(
;;knd = 0
  (tmpret0 #f)
  (tmp1 #f)
  (tmp2 #f)
  (tmp3 #f)
  (tmp4 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_qlistref_make_nil
  (ATSINStmpset tmp2 atscc2scm_null)
  (ATSINStmpset tmp1 (ats2scmpre_ref tmp2))
  (ATSINStmpset tmp4 atscc2scm_null)
  (ATSINStmpset tmp3 (ats2scmpre_ref tmp4))
  (ATSPMVtysum tmp1 tmp3)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_qlistref_length arg0)
(let(
;;knd = 0
  (tmpret5 #f)
  (tmp6 #f)
  (tmp7 #f)
  (tmp8 #f)
  (tmp9 #f)
  (tmp10 #f)
  (tmp11 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_qlistref_length
  (ATSINStmpset tmp6 (ATSSELcon arg0 0))
  (ATSINStmpset tmp7 (ATSSELcon arg0 1))
  (ATSINStmpset tmp9 (ats2scmpre_ref_get_elt tmp6))
  (ATSINStmpset tmp8 (ats2scmpre_list_length tmp9))
  (ATSINStmpset tmp11 (ats2scmpre_ref_get_elt tmp7))
  (ATSINStmpset tmp10 (ats2scmpre_list_length tmp11))
  (ats2scmpre_add_int1_int1 tmp8 tmp10)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_qlistref_enqueue arg0 arg1)
(let(
;;knd = 0
  (tmp13 #f)
  (tmp14 #f)
  (tmp15 #f)
  (tmp16 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_qlistref_enqueue
  (ATSINStmpset tmp13 (ATSSELcon arg0 0))
  (ATSINStmpset tmp14 (ATSSELcon arg0 1))
  (ATSINStmpset tmp16 (ats2scmpre_ref_get_elt tmp13))
  (ATSINStmpset tmp15 (ATSPMVtysum arg1 tmp16))
  (ATSINSmove1_void (ats2scmpre_ref_set_elt tmp13 tmp15))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_qlistref_dequeue_opt arg0)
(let(
;;knd = 0
  (tmpret17 #f)
  (tmp18 #f)
  (tmp19 #f)
  (tmp20 #f)
  (tmp21 #f)
  (tmp22 #f)
  (tmp23 #f)
  (tmp25 #f)
  (tmp26 #f)
  (tmp27 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_qlistref_dequeue_opt
  (ATSINStmpset tmp18 (ATSSELcon arg0 0))
  (ATSINStmpset tmp19 (ATSSELcon arg0 1))
  (ATSINStmpset tmp20 (ats2scmpre_ref_get_elt tmp19))
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons tmp20)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       (ATSINStmpset tmp23 (ats2scmpre_ref_get_elt tmp18))
       (ATSINStmpset tmp25 atscc2scm_null)
       (ATSINSmove1_void (ats2scmpre_ref_set_elt tmp18 tmp25))
       (letrec(
         (casefnx
          (lambda(tmplab)(case tmplab
           ;; ATSbranchseq_beg
           ((1)
            (if (ATSCKptriscons tmp23)
              (casefnx 4)
              (begin
               (casefnx 2)
              )
            )
           ) ;; end-of-branch
           ((2)
            atscc2scm_null
           ) ;; end-of-branch
           ;; ATSbranchseq_end
           ;; ATSbranchseq_beg
           ((3)
            (casefnx 4)
           ) ;; end-of-branch
           ((4)
            (ATSINStmpset tmp26 (ATSSELcon tmp23 0))
            (ATSINStmpset tmp27 (ATSSELcon tmp23 1))
            (ATSINSmove1_void (ats2scmpre_ref_set_elt tmp19 tmp27))
            (ATSPMVtysum tmp26)
           ) ;; end-of-branch
           ;; ATSbranchseq_end
           ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
           ) ;; end-of-case
          ) ;; end-of-lambda
         ) ;; end-of-casefnx
        ) (casefnx 1)
       ) ;; end-of-letrec
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp21 (ATSSELcon tmp20 0))
       (ATSINStmpset tmp22 (ATSSELcon tmp20 1))
       (ATSINSmove1_void (ats2scmpre_ref_set_elt tmp19 tmp22))
       (ATSPMVtysum tmp21)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_qlistref_foldleft arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret30 #f)
  (tmp31 #f)
  (tmp32 #f)
  (tmp41 #f)
  (tmp42 #f)
  (tmp43 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_qlistref_foldleft
  (ATSINStmpset tmp31 (ATSSELcon arg0 0))
  (ATSINStmpset tmp32 (ATSSELcon arg0 1))
  (ATSINStmpset tmp41 (ats2scmpre_ref_get_elt tmp31))
  (ATSINStmpset tmp43 (ats2scmpre_ref_get_elt tmp32))
  (ATSINStmpset tmp42 (_ats2scmpre_qlistref_auxl_5 arg2 arg1 tmp43))
  (_ats2scmpre_qlistref_auxr_6 arg2 tmp41 tmp42)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_qlistref_auxl_5 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret33 #f)
  (tmp34 #f)
  (tmp35 #f)
  (tmp36 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_qlistref_auxl_5
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg1)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg0
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp34 (ATSSELcon arg1 0))
         (ATSINStmpset tmp35 (ATSSELcon arg1 1))
         (ATSINStmpset tmp36 ((ATSfunclo_fclo env0) env0 arg0 tmp34))
         ;; apy0 = tmp36
         ;; apy1 = tmp35
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_qlistref_auxl_5
         (_ats2scmpre_qlistref_auxl_5 env0 tmp36 tmp35)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret33;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_qlistref_auxr_6 env0 arg0 arg1)
(let(
;;knd = 0
  (tmpret37 #f)
  (tmp38 #f)
  (tmp39 #f)
  (tmp40 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_qlistref_auxr_6
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       arg1
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp38 (ATSSELcon arg0 0))
       (ATSINStmpset tmp39 (ATSSELcon arg0 1))
       (ATSINStmpset tmp40 (_ats2scmpre_qlistref_auxr_6 env0 tmp39 arg1))
       ((ATSfunclo_fclo env0) env0 tmp40 tmp38)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_qlistref_foldright arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret44 #f)
  (tmp45 #f)
  (tmp46 #f)
  (tmp55 #f)
  (tmp56 #f)
  (tmp57 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_qlistref_foldright
  (ATSINStmpset tmp45 (ATSSELcon arg0 0))
  (ATSINStmpset tmp46 (ATSSELcon arg0 1))
  (ATSINStmpset tmp55 (ats2scmpre_ref_get_elt tmp46))
  (ATSINStmpset tmp57 (ats2scmpre_ref_get_elt tmp45))
  (ATSINStmpset tmp56 (_ats2scmpre_qlistref_auxl_8 arg1 arg2 tmp57))
  (_ats2scmpre_qlistref_auxr_9 arg1 tmp55 tmp56)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_qlistref_auxl_8 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret47 #f)
  (tmp48 #f)
  (tmp49 #f)
  (tmp50 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_qlistref_auxl_8
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg1)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg0
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp48 (ATSSELcon arg1 0))
         (ATSINStmpset tmp49 (ATSSELcon arg1 1))
         (ATSINStmpset tmp50 ((ATSfunclo_fclo env0) env0 tmp48 arg0))
         ;; apy0 = tmp50
         ;; apy1 = tmp49
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_qlistref_auxl_8
         (_ats2scmpre_qlistref_auxl_8 env0 tmp50 tmp49)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret47;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_qlistref_auxr_9 env0 arg0 arg1)
(let(
;;knd = 0
  (tmpret51 #f)
  (tmp52 #f)
  (tmp53 #f)
  (tmp54 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_qlistref_auxr_9
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       arg1
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp52 (ATSSELcon arg0 0))
       (ATSINStmpset tmp53 (ATSSELcon arg0 1))
       (ATSINStmpset tmp54 (_ats2scmpre_qlistref_auxr_9 env0 tmp53 arg1))
       ((ATSfunclo_fclo env0) env0 tmp52 tmp54)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun

;;;;;;
;;
;; end-of-compilation-unit
;;
;;;;;;
;;;;;;
;;
;; The Scheme code is generated by atscc2scm
;; The starting compilation time is: 2017-4-12:  0h:35m
;;
;;;;;;

(define
(_ats2scmpre_ML_list0_patsfun_29__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_29 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_ML_list0_patsfun_32__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_32 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_ML_list0_patsfun_35__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_35 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_ML_list0_patsfun_38__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_38 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_ML_list0_patsfun_42__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_42 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_ML_list0_patsfun_45__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_45 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_ML_list0_patsfun_48__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_48 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_ML_list0_patsfun_51__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_51 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_ML_list0_patsfun_54__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_54 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_ML_list0_patsfun_58__closurerize xenv0)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_58 (ATSCCget_1 _fcenvs_) xarg0)) xenv0)
;;%}
) ;; define


(define
(_ats2scmpre_ML_list0_patsfun_64__closurerize xenv0 xenv1)
;;%{
  (list (lambda(_fcenvs_ xarg0)(_ats2scmpre_ML_list0_patsfun_64 (ATSCCget_1 _fcenvs_) (ATSCCget_2 _fcenvs_) xarg0)) xenv0 xenv1)
;;%}
) ;; define


;;fun
(define
(ats2scmpre_ML_list0_head_opt arg0)
(let(
;;knd = 0
  (tmpret7 #f)
  (tmp8 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_head_opt
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp8 (ATSSELcon arg0 0))
       (ATSPMVtysum tmp8)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_tail_opt arg0)
(let(
;;knd = 0
  (tmpret10 #f)
  (tmp12 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_tail_opt
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp12 (ATSSELcon arg0 1))
       (ATSPMVtysum tmp12)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_length arg0)
(let(
;;knd = 0
  (tmpret13 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_length
  (ats2scmpre_list_length arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_last_opt arg0)
(let(
;;knd = 0
  (tmpret14 #f)
  (tmp18 #f)
  (tmp19 #f)
  (tmp20 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_last_opt
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp18 (ATSSELcon arg0 0))
       (ATSINStmpset tmp19 (ATSSELcon arg0 1))
       (ATSINStmpset tmp20 (_ats2scmpre_ML_list0_loop_8 tmp18 tmp19))
       (ATSPMVtysum tmp20)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_loop_8 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret15 #f)
  (tmp16 #f)
  (tmp17 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_ML_list0_loop_8
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg1)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg0
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp16 (ATSSELcon arg1 0))
         (ATSINStmpset tmp17 (ATSSELcon arg1 1))
         ;; apy0 = tmp16
         ;; apy1 = tmp17
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_ML_list0_loop_8
         (_ats2scmpre_ML_list0_loop_8 tmp16 tmp17)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret15;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_get_at_opt arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret21 #f)
  (tmp22 #f)
  (tmp23 #f)
  (tmp24 #f)
  (tmp25 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_list0_get_at_opt
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg0)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_null
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp22 (ATSSELcon arg0 0))
         (ATSINStmpset tmp23 (ATSSELcon arg0 1))
         (ATSINStmpset tmp24 (ats2scmpre_gt_int1_int1 arg1 0))
         (if tmp24
           (begin
            (ATSINStmpset tmp25 (ats2scmpre_sub_int1_int1 arg1 1))
            ;; apy0 = tmp23
            ;; apy1 = tmp25
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; funlab_scm = 1; // __patsflab_list0_get_at_opt
            (ats2scmpre_ML_list0_get_at_opt tmp23 tmp25)
           ) ;; if-then
           (begin
            (ATSPMVtysum tmp22)
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret21;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_make_elt arg0 arg1)
(let(
;;knd = 0
  (tmpret26 #f)
  (tmp27 #f)
  (tmp28 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_make_elt
  (ATSINStmpset tmp27 (ats2scmpre_gte_int1_int1 arg0 0))
  (if tmp27
    (begin
     (ATSINStmpset tmp28 (ats2scmpre_list_make_elt arg0 arg1))
     tmp28
    ) ;; if-then
    (begin
     atscc2scm_null
    ) ;; if-else
  )
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_make_intrange_2 arg0 arg1)
(let(
;;knd = 0
  (tmpret29 #f)
  (tmp30 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_make_intrange_2
  (ATSINStmpset tmp30 (ats2scmpre_list_make_intrange_2 arg0 arg1))
  tmp30
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_make_intrange_3 arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret31 #f)
  (tmp32 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_make_intrange_3
  (ATSINStmpset tmp32 (ats2scmpre_list_make_intrange_3 arg0 arg1 arg2))
  tmp32
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_snoc arg0 arg1)
(let(
;;knd = 0
  (tmpret44 #f)
  (tmp45 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_snoc
  (ATSINStmpset tmp45 (ats2scmpre_list_snoc arg0 arg1))
  tmp45
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_extend arg0 arg1)
(let(
;;knd = 0
  (tmpret46 #f)
  (tmp47 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_extend
  (ATSINStmpset tmp47 (ats2scmpre_list_extend arg0 arg1))
  tmp47
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_append arg0 arg1)
(let(
;;knd = 0
  (tmpret48 #f)
  (tmp49 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_append
  (ATSINStmpset tmp49 (ats2scmpre_list_append arg0 arg1))
  tmp49
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_mul_int_list0 arg0 arg1)
(let(
;;knd = 0
  (tmpret50 #f)
  (tmp51 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_mul_int_list0
  (ATSINStmpset tmp51 (ats2scmpre_mul_int_list arg0 arg1))
  tmp51
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_reverse arg0)
(let(
;;knd = 0
  (tmpret52 #f)
  (tmp53 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_reverse
  (ATSINStmpset tmp53 (ats2scmpre_list_reverse arg0))
  tmp53
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_reverse_append arg0 arg1)
(let(
;;knd = 0
  (tmpret54 #f)
  (tmp55 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_reverse_append
  (ATSINStmpset tmp55 (ats2scmpre_list_reverse_append arg0 arg1))
  tmp55
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_concat arg0)
(let(
;;knd = 0
  (tmpret56 #f)
  (tmp57 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_concat
  (ATSINStmpset tmp57 (ats2scmpre_list_concat arg0))
  tmp57
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_remove_at_opt arg0 arg1)
(let(
;;knd = 0
  (tmpret58 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_remove_at_opt
  (_ats2scmpre_ML_list0_aux_26 arg0 0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_aux_26 arg0 arg1)
(let(
;;knd = 0
  (tmpret59 #f)
  (tmp60 #f)
  (tmp61 #f)
  (tmp62 #f)
  (tmp63 #f)
  (tmp64 #f)
  (tmp65 #f)
  (tmp66 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_aux_26
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp60 (ATSSELcon arg0 0))
       (ATSINStmpset tmp61 (ATSSELcon arg0 1))
       (ATSINStmpset tmp62 (ats2scmpre_gt_int1_int1 arg1 0))
       (if tmp62
         (begin
          (ATSINStmpset tmp64 (ats2scmpre_sub_int1_int1 arg1 1))
          (ATSINStmpset tmp63 (_ats2scmpre_ML_list0_aux_26 tmp61 tmp64))
          (letrec(
            (casefnx
             (lambda(tmplab)(case tmplab
              ;; ATSbranchseq_beg
              ((1)
               (if (ATSCKptriscons tmp63)
                 (casefnx 4)
                 (begin
                  (casefnx 2)
                 )
               )
              ) ;; end-of-branch
              ((2)
               atscc2scm_null
              ) ;; end-of-branch
              ;; ATSbranchseq_end
              ;; ATSbranchseq_beg
              ((3)
               (casefnx 4)
              ) ;; end-of-branch
              ((4)
               (ATSINStmpset tmp65 (ATSSELcon tmp63 0))
               ;; ATSINSfreecon(tmp63);
               (ATSINStmpset tmp66 (ATSPMVtysum tmp60 tmp65))
               (ATSPMVtysum tmp66)
              ) ;; end-of-branch
              ;; ATSbranchseq_end
              ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
              ) ;; end-of-case
             ) ;; end-of-lambda
            ) ;; end-of-casefnx
           ) (casefnx 1)
          ) ;; end-of-letrec
         ) ;; if-then
         (begin
          (ATSPMVtysum tmp61)
         ) ;; if-else
       )
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_exists arg0 arg1)
(let(
;;knd = 0
  (tmpret67 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_exists
  (ats2scmpre_list_exists arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_exists_method arg0)
(let(
;;knd = 0
  (tmpret68 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_exists_method
  (_ats2scmpre_ML_list0_patsfun_29__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_29 env0 arg0)
(let(
;;knd = 0
  (tmpret69 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_29
  (ats2scmpre_ML_list0_exists env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_iexists arg0 arg1)
(let(
;;knd = 0
  (tmpret70 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_iexists
  (ats2scmpre_list_iexists arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_iexists_method arg0)
(let(
;;knd = 0
  (tmpret71 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_iexists_method
  (_ats2scmpre_ML_list0_patsfun_32__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_32 env0 arg0)
(let(
;;knd = 0
  (tmpret72 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_32
  (ats2scmpre_ML_list0_iexists env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_forall arg0 arg1)
(let(
;;knd = 0
  (tmpret73 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_forall
  (ats2scmpre_list_forall arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_forall_method arg0)
(let(
;;knd = 0
  (tmpret74 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_forall_method
  (_ats2scmpre_ML_list0_patsfun_35__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_35 env0 arg0)
(let(
;;knd = 0
  (tmpret75 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_35
  (ats2scmpre_ML_list0_forall env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_iforall arg0 arg1)
(let(
;;knd = 0
  (tmpret76 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_iforall
  (ats2scmpre_list_iforall arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_iforall_method arg0)
(let(
;;knd = 0
  (tmpret77 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_iforall_method
  (_ats2scmpre_ML_list0_patsfun_38__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_38 env0 arg0)
(let(
;;knd = 0
  (tmpret78 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_38
  (ats2scmpre_ML_list0_iforall env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_app arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_app
  (ATSINSmove1_void (ats2scmpre_ML_list0_foreach arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_foreach arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_foreach
  (ATSINSmove1_void (ats2scmpre_list_foreach arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_foreach_method arg0)
(let(
;;knd = 0
  (tmpret81 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_foreach_method
  (_ats2scmpre_ML_list0_patsfun_42__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_42 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_42
  (ATSINSmove1_void (ats2scmpre_ML_list0_foreach env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_iforeach arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_iforeach
  (ATSINSmove1_void (ats2scmpre_list_iforeach arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_iforeach_method arg0)
(let(
;;knd = 0
  (tmpret84 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_iforeach_method
  (_ats2scmpre_ML_list0_patsfun_45__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_45 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_45
  (ATSINSmove1_void (ats2scmpre_ML_list0_iforeach env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_rforeach arg0 arg1)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_rforeach
  (ATSINSmove1_void (ats2scmpre_list_rforeach arg0 arg1))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_rforeach_method arg0)
(let(
;;knd = 0
  (tmpret87 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_rforeach_method
  (_ats2scmpre_ML_list0_patsfun_48__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_48 env0 arg0)
(let(
;;knd = 0
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_48
  (ATSINSmove1_void (ats2scmpre_ML_list0_rforeach env0 arg0))
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_filter arg0 arg1)
(let(
;;knd = 0
  (tmpret89 #f)
  (tmp90 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_filter
  (ATSINStmpset tmp90 (ats2scmpre_list_filter arg0 arg1))
  tmp90
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_filter_method arg0)
(let(
;;knd = 0
  (tmpret91 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_filter_method
  (_ats2scmpre_ML_list0_patsfun_51__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_51 env0 arg0)
(let(
;;knd = 0
  (tmpret92 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_51
  (ats2scmpre_ML_list0_filter env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_map arg0 arg1)
(let(
;;knd = 0
  (tmpret93 #f)
  (tmp94 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_map
  (ATSINStmpset tmp94 (ats2scmpre_list_map arg0 arg1))
  tmp94
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_map_method arg0 arg1)
(let(
;;knd = 0
  (tmpret95 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_map_method
  (_ats2scmpre_ML_list0_patsfun_54__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_54 env0 arg0)
(let(
;;knd = 0
  (tmpret96 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_54
  (ats2scmpre_ML_list0_map env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_mapcons arg0 arg1)
(let(
;;knd = 0
  (tmpret97 #f)
  (tmp98 #f)
  (tmp99 #f)
  (tmp100 #f)
  (tmp101 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_mapcons
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg1)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp98 (ATSSELcon arg1 0))
       (ATSINStmpset tmp99 (ATSSELcon arg1 1))
       (ATSINStmpset tmp100 (ATSPMVtysum arg0 tmp98))
       (ATSINStmpset tmp101 (ats2scmpre_ML_list0_mapcons arg0 tmp99))
       (ATSPMVtysum tmp100 tmp101)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_find_opt arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret102 #f)
  (tmp103 #f)
  (tmp104 #f)
  (tmp105 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab_list0_find_opt
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg0)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         atscc2scm_null
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp103 (ATSSELcon arg0 0))
         (ATSINStmpset tmp104 (ATSSELcon arg0 1))
         (ATSINStmpset tmp105 ((ATSfunclo_fclo arg1) arg1 tmp103))
         (if tmp105
           (begin
            (ATSPMVtysum tmp103)
           ) ;; if-then
           (begin
            ;; apy0 = tmp104
            ;; apy1 = arg1
            ;; arg0 = apy0
            ;; arg1 = apy1
            ;; funlab_scm = 1; // __patsflab_list0_find_opt
            (ats2scmpre_ML_list0_find_opt tmp104 arg1)
           ) ;; if-else
         )
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret102;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_find_opt_method arg0)
(let(
;;knd = 0
  (tmpret106 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_find_opt_method
  (_ats2scmpre_ML_list0_patsfun_58__closurerize arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_58 env0 arg0)
(let(
;;knd = 0
  (tmpret107 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_58
  (ats2scmpre_ML_list0_find_opt env0 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_zip arg0 arg1)
(let(
;;knd = 0
  (tmpret108 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_zip
  (_ats2scmpre_ML_list0_aux_60 arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_aux_60 arg0 arg1)
(let(
;;knd = 0
  (tmpret109 #f)
  (tmp110 #f)
  (tmp111 #f)
  (tmp112 #f)
  (tmp113 #f)
  (tmp114 #f)
  (tmp115 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_aux_60
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp110 (ATSSELcon arg0 0))
       (ATSINStmpset tmp111 (ATSSELcon arg0 1))
       (letrec(
         (casefnx
          (lambda(tmplab)(case tmplab
           ;; ATSbranchseq_beg
           ((1)
            (if (ATSCKptriscons arg1)
              (casefnx 4)
              (begin
               (casefnx 2)
              )
            )
           ) ;; end-of-branch
           ((2)
            atscc2scm_null
           ) ;; end-of-branch
           ;; ATSbranchseq_end
           ;; ATSbranchseq_beg
           ((3)
            (casefnx 4)
           ) ;; end-of-branch
           ((4)
            (ATSINStmpset tmp112 (ATSSELcon arg1 0))
            (ATSINStmpset tmp113 (ATSSELcon arg1 1))
            (ATSINStmpset tmp114 (ATSPMVtyrec tmp110 tmp112))
            (ATSINStmpset tmp115 (_ats2scmpre_ML_list0_aux_60 tmp111 tmp113))
            (ATSPMVtysum tmp114 tmp115)
           ) ;; end-of-branch
           ;; ATSbranchseq_end
           ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
           ) ;; end-of-case
          ) ;; end-of-lambda
         ) ;; end-of-casefnx
        ) (casefnx 1)
       ) ;; end-of-letrec
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_zipwith arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret116 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_zipwith
  (_ats2scmpre_ML_list0_aux_62 arg0 arg1 arg2)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_aux_62 arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret117 #f)
  (tmp118 #f)
  (tmp119 #f)
  (tmp120 #f)
  (tmp121 #f)
  (tmp122 #f)
  (tmp123 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_aux_62
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       atscc2scm_null
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp118 (ATSSELcon arg0 0))
       (ATSINStmpset tmp119 (ATSSELcon arg0 1))
       (letrec(
         (casefnx
          (lambda(tmplab)(case tmplab
           ;; ATSbranchseq_beg
           ((1)
            (if (ATSCKptriscons arg1)
              (casefnx 4)
              (begin
               (casefnx 2)
              )
            )
           ) ;; end-of-branch
           ((2)
            atscc2scm_null
           ) ;; end-of-branch
           ;; ATSbranchseq_end
           ;; ATSbranchseq_beg
           ((3)
            (casefnx 4)
           ) ;; end-of-branch
           ((4)
            (ATSINStmpset tmp120 (ATSSELcon arg1 0))
            (ATSINStmpset tmp121 (ATSSELcon arg1 1))
            (ATSINStmpset tmp122 ((ATSfunclo_fclo arg2) arg2 tmp118 tmp120))
            (ATSINStmpset tmp123 (_ats2scmpre_ML_list0_aux_62 tmp119 tmp121 arg2))
            (ATSPMVtysum tmp122 tmp123)
           ) ;; end-of-branch
           ;; ATSbranchseq_end
           ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
           ) ;; end-of-case
          ) ;; end-of-lambda
         ) ;; end-of-casefnx
        ) (casefnx 1)
       ) ;; end-of-letrec
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_zipwith_method arg0 arg1)
(let(
;;knd = 0
  (tmpret124 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_zipwith_method
  (_ats2scmpre_ML_list0_patsfun_64__closurerize arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_patsfun_64 env0 env1 arg0)
(let(
;;knd = 0
  (tmpret125 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_patsfun_64
  (ats2scmpre_ML_list0_zipwith env0 env1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_foldleft arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret126 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_foldleft
  (_ats2scmpre_ML_list0_aux_66 arg2 arg1 arg0)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_aux_66 env0 arg0 arg1)
(let(
;;knd = 1
;;(apy0 #f)
;;(apy1 #f)
  (tmpret127 #f)
  (tmp128 #f)
  (tmp129 #f)
  (tmp130 #f)
;;var funlab_scm
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;while(true) {
    ;; funlab_scm = 0;
    ;; __patsflab__ats2scmpre_ML_list0_aux_66
    (letrec(
      (casefnx
       (lambda(tmplab)(case tmplab
        ;; ATSbranchseq_beg
        ((1)
         (if (ATSCKptriscons arg1)
           (casefnx 4)
           (begin
            (casefnx 2)
           )
         )
        ) ;; end-of-branch
        ((2)
         arg0
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; ATSbranchseq_beg
        ((3)
         (casefnx 4)
        ) ;; end-of-branch
        ((4)
         (ATSINStmpset tmp128 (ATSSELcon arg1 0))
         (ATSINStmpset tmp129 (ATSSELcon arg1 1))
         (ATSINStmpset tmp130 ((ATSfunclo_fclo env0) env0 arg0 tmp128))
         ;; apy0 = tmp130
         ;; apy1 = tmp129
         ;; arg0 = apy0
         ;; arg1 = apy1
         ;; funlab_scm = 1; // __patsflab__ats2scmpre_ML_list0_aux_66
         (_ats2scmpre_ML_list0_aux_66 env0 tmp130 tmp129)
        ) ;; end-of-branch
        ;; ATSbranchseq_end
        ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
        ) ;; end-of-case
       ) ;; end-of-lambda
      ) ;; end-of-casefnx
     ) (casefnx 1)
    ) ;; end-of-letrec
    ;; if (funlab_scm > 0) continue; else ;; return tmpret127;
  ;} // endwhile-fun
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_foldright arg0 arg1 arg2)
(let(
;;knd = 0
  (tmpret131 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_foldright
  (_ats2scmpre_ML_list0_aux_68 arg1 arg2 arg0 arg2)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(_ats2scmpre_ML_list0_aux_68 env0 env1 arg0 arg1)
(let(
;;knd = 0
  (tmpret132 #f)
  (tmp133 #f)
  (tmp134 #f)
  (tmp135 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab__ats2scmpre_ML_list0_aux_68
  (letrec(
    (casefnx
     (lambda(tmplab)(case tmplab
      ;; ATSbranchseq_beg
      ((1)
       (if (ATSCKptriscons arg0)
         (casefnx 4)
         (begin
          (casefnx 2)
         )
       )
      ) ;; end-of-branch
      ((2)
       arg1
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; ATSbranchseq_beg
      ((3)
       (casefnx 4)
      ) ;; end-of-branch
      ((4)
       (ATSINStmpset tmp133 (ATSSELcon arg0 0))
       (ATSINStmpset tmp134 (ATSSELcon arg0 1))
       (ATSINStmpset tmp135 (_ats2scmpre_ML_list0_aux_68 env0 env1 tmp134 env1))
       ((ATSfunclo_fclo env0) env0 tmp133 tmp135)
      ) ;; end-of-branch
      ;; ATSbranchseq_end
      ;; (else (atscc2scm_caseof_deadcode _FILE_ _LINE_))
      ) ;; end-of-case
     ) ;; end-of-lambda
    ) ;; end-of-casefnx
   ) (casefnx 1)
  ) ;; end-of-letrec
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_list0_sort_2 arg0 arg1)
(let(
;;knd = 0
  (tmpret138 #f)
  (tmp139 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_list0_sort_2
  (ATSINStmpset tmp139 (ats2scmpre_list_sort_2 arg0 arg1))
  tmp139
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_streamize_list0_zip arg0 arg1)
(let(
;;knd = 0
  (tmpret140 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_streamize_list0_zip
  (ats2scmpre_streamize_list_zip arg0 arg1)
) ;; end-of-let
) ;; end-of-fun


;;fun
(define
(ats2scmpre_ML_streamize_list0_cross arg0 arg1)
(let(
;;knd = 0
  (tmpret141 #f)
;;var tmplab,tmplab_scm
) ;; in-of-let
  ;; __patsflab_streamize_list0_cross
  (ats2scmpre_streamize_list_cross arg0 arg1)
) ;; end-of-let
) ;; end-of-fun

;;;;;;
;;
;; end-of-compilation-unit
;;
;;;;;;

;; ****** ****** ;;

;; end of [libatscc2scm_all.scm] ;;
