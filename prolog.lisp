;; Can BEYAZNAR
;; 161044038


(setq rule-list '())
(setq fact-list '())
(setq query-list '())

;; Reads allt the charecters in file 
;; and appends to the list
(defun read-file(filename)

    (let ( (file-string '()) (in (open filename :if-does-not-exist nil)  ))
        (when in
            (loop for tempChar = (read-char in nil)
                while tempChar do
                
                (if (not (or (eq tempChar #\Space) (eq tempChar #\Newline))) (setq file-string (append file-string (list (string tempChar)))))           
            )			
            (close in)
        )
        (return-from read-file file-string)
    )
)

;; merge strings in the list and collect them in a single string
;; example #\" "h" "o" "r" "s" "e" #\" ---> "horse"
(defun merge-string-variables (user-list) 
    (let ( (temp-list '()) (cur-index 0) (len-list (length user-list)) (string-list nil) )
        (loop
            (when (>= cur-index len-list) (return ))
            (cond 
                (
                    (string= (nth cur-index user-list) "\"")
                    (setq cur-index (+ cur-index 1))
                    (setq string-list "")
                    (loop
                        (when (>= cur-index len-list) (return ))
                        (if (string= (nth cur-index user-list) "\"")
                            (progn 
                                (setq temp-list (append temp-list (list string-list)))
                                (return )
                            )
                        )
                        (setq string-list (concatenate 'string string-list (nth cur-index user-list)))
                        (setq cur-index (+ cur-index 1))   
                    )
                )
                (t
                    (setq temp-list (append temp-list (list (nth cur-index user-list))))
                )
            )
            (setq cur-index (+ cur-index 1))
        )
        (return-from merge-string-variables temp-list)
    )
    

)

;; Convert list format to the lisp
;; I used the stack logic here, the values are pushed into the list until the closed parentheses come. 
;; If closed parentheses appear, pop up until the open parenthesis appears, 
;; and each value is thrown to another list. 
;; After the open parenthesis is displayed, the other list is pushed back to the original list. 
;; Thus, the brackets format in the input file is converted into a list without breaking.
(defun convert-string-to-list (str-input) 
    (let ( (result-list '()) (temp-list nil) 
    (len-input (length str-input)) (temp-val nil) (cur-index 0) (last-list '())  )

        (loop
            (when (= cur-index len-input) (return))
            ;; pop values while read (
            ;; if (nth cur-index str-input) is list and equals ")"
            (if (string= ")" (nth cur-index str-input) ) 
                (progn
                    
                    (setq temp-list '())
                    (loop 
                        (setq temp-val (pop result-list))
                        
                        (if (and (not (listp temp-val)) (string= temp-val "(") )
                            (progn
                                (push temp-list result-list )
                                (return )
                            )
                            (progn
                                (if (or (listp temp-val) (not (string= temp-val ")"))) 
                                    (push temp-val temp-list)
                                ))
                        )
                    )
                )
                (push  (nth cur-index str-input) result-list )
            )
            (setq cur-index (+ cur-index 1)))
        (return-from convert-string-to-list (nth 0 result-list)))
)

;; Returns true if the first character of string is uppercase. 
;; NOTE: it will not work correctly for numbers!!
(defun is-first-char-uppercase (val)
    (if (string= (char val 0) (char-upcase (char val 0)))
        (return-from is-first-char-uppercase T)
        (return-from is-first-char-uppercase NIL)
    )
)

;; Returns true if the first character of string is digit. 
(defun is-first-char-digit (val)
    (if (eq nil (digit-char-p (char val 0)))
        (return-from is-first-char-digit NIL)
        (return-from is-first-char-digit T)
    )
)

;; Searches for rules that match the query. And it returns the list of rules that match.
(defun find-match-rules (rule-name parameter-list)

    (let ( (cur-index 0) (len-rule-list (length rule-list)) (cur-rule nil)
            (cur-rule-name nil) (cur-rule-params nil) (cur-rule-results nil) (rule-index-list '()) ) 
    
        (loop 
            (when (>= cur-index len-rule-list) (return ))
            (setq cur-rule (nth cur-index rule-list))
            (setq cur-rule-name (nth 0 (nth 0 cur-rule)))
            (setq cur-rule-params (nth 1 (nth 0 cur-rule)))
            (setq cur-rule-results (nth 1 cur-rule) )
            
            ;; if rule name is same and parameter count is equal
            (if (and  (string= rule-name cur-rule-name) (= (length cur-rule-params) (length parameter-list) ) )
            
                (progn
                    (let ( (cur-param-index 0) (len-param (length cur-rule-params)) (correct-param-count 0) )
                        (loop 
                            (when (>= cur-param-index len-param) (return ))       
                            (cond            
                                (   
                                    ;; for parameters like "X" or "Can"
                                    (and (is-first-char-uppercase (nth cur-param-index cur-rule-params) )
                                    (not (is-first-char-digit (nth cur-param-index cur-rule-params))))
                                    (setq correct-param-count (+ correct-param-count 1))
                                )
                                (   
                                    ;; for integer values
                                    (and (is-first-char-digit (nth cur-param-index cur-rule-params)) 
                                        (string= (nth cur-param-index cur-rule-params) (nth cur-param-index parameter-list) ))
                                    (setq correct-param-count (+ correct-param-count 1))
                                )   
                                (t 
                                    (return )
                                )
                            )
                            (if (= correct-param-count len-param)
                                (progn
                                    (setq rule-index-list (append rule-index-list (list cur-index)))
                                    (return )
                                )
                            )
                            (setq cur-param-index (+ cur-param-index 1))
                        )

                        
                        
                    )
                )
            )
            (setq cur-index (+ cur-index 1))
        )
        (return-from find-match-rules rule-index-list)
    )

)

;; Searches for facts that match the query. And it returns the list of facts that match.
(defun find-match-facts (fact-name parameter-list)
    (let ( (cur-index 0) (len-fact-list (length fact-list)) 
        (cur-fact nil) (cur-fact-name nil) (cur-fact-parameters nil) (facts-indexes '()))

        (loop 
            (when (>= cur-index len-fact-list) (return ))

            (setq cur-fact (nth 0 (nth cur-index fact-list)) )
            (setq cur-fact-name (nth 0 cur-fact ))
            (setq cur-fact-parameters (nth 1 cur-fact ))
            (if (and (string= cur-fact-name fact-name) (= (length cur-fact-parameters ) (length parameter-list)))
                (progn            
                    (let ((cur-param-index 0) (len-parameter (length cur-fact-parameters)) (match-parameter-count 0)) 

                        (loop 
                            (when (>= cur-param-index len-parameter) (return ))
                            
                            (if (string= (nth cur-param-index parameter-list) (nth cur-param-index cur-fact-parameters))
                                (setq match-parameter-count (+ match-parameter-count 1))
                                (return )
                            )

                            (setq cur-param-index (+ cur-param-index 1))
                        )
                        (if (= match-parameter-count len-parameter)
                            (setq facts-indexes (append facts-indexes (list cur-param-index)))
                            ;; (return-from find-match-facts cur-param-index)
                        ) 
                    )
                )
            )
            (setq cur-index (+ cur-index 1))
        )
        (return-from find-match-facts facts-indexes)
    )
)

;; subst implementation
;; i did not use this function
;; (defun my-subst (new-param old-param user-list)
;;     (cond 
;;         (
;;             (equal old-param user-list)
;;             new-param
;;         )
;;         (
;;             (atom user-list)
;;             user-list
;;         )
;;         (t
;;             (cons (my-subst new-param old-param (car user-list))
;;             (my-subst new-param old-param (cdr user-list)))
;;         )
;;     )
;; )

;; applies a query according to a specified rule. 
;; Here, parameters are adjusted according to the query and evaluate query function is called.
;; If the values of all functions in the body part of the rule come true from evaluate-query, this function returns true.
(defun evaluate-query-for-rule (query-input rule-index) 

    (let ( (cur-index 0) (parameter-count (length (nth 1 query-input))) 
            (current-rule (nth rule-index rule-list))
            (query-parameters (nth 1 query-input))
            (current-rule-head-params nil) (current-rule-body nil)
            (changed-rule nil) (body-results '()) (true-fact-count 0) (temp-val nil) )

        (setq current-rule-body (nth 1 current-rule))
        (setq current-rule-head-params (nth 1 (nth 0 current-rule)))    

        ;; Change parameters in rule
        ;; after the loop evaluate body part of rule
        (loop
            (when (>= cur-index parameter-count) (return ))

            (if (and (is-first-char-uppercase (nth cur-index current-rule-head-params)) 
                        (not (is-first-char-digit (nth cur-index current-rule-head-params)) ))
                (progn 
                    ;;(setq changed-rule (my-subst (nth cur-index query-parameters) (nth cur-index current-rule-head-params) current-rule ))
                    (setq changed-rule (subst (nth cur-index query-parameters) (nth cur-index current-rule-head-params) current-rule :test #'equal ))
                )
            )
            (setq cur-index (+ cur-index 1))
        )
        
        (if (eq nil changed-rule)
            (setq changed-rule current-rule)
        )
        (setq cur-index 0)
        (setq current-rule-body (nth 1 changed-rule))
        (setq parameter-count (length current-rule-body))
        (loop 
            (when (>= cur-index parameter-count) (return ))

            (setq temp-val (evaluate-query (nth cur-index current-rule-body)))
            (setq body-results (append body-results (list temp-val)) )
            (if (equal temp-val t)
                (setq true-fact-count (+ true-fact-count 1))
                (return-from evaluate-query-for-rule nil)
            )
            (setq cur-index (+ cur-index 1))
        )
        (if (equal true-fact-count parameter-count)
            (return-from evaluate-query-for-rule t)
        )
        (return-from evaluate-query-for-rule nil)
    )
)

;; Here, first of all, it is checked whether the query that comes as input matches with facts.
;; returns true if they match. If not, it is checked if it meets the rules.
;; evaluate-query-for-rule function is called for the rules that agree.
(defun evaluate-query (query-input)
    (let ( (function-val query-input ) (function-name nil) (function-variables nil)
            (match-rule-indexes nil) (match-fact-indexes nil) )

        (setq function-name  (nth 0 function-val))
        (setq function-variables (nth 1 function-val))
        
        (setq match-fact-indexes (find-match-facts function-name function-variables ) )
        (if (not (eq nil match-fact-indexes)) (return-from evaluate-query t))

        (setq match-rule-indexes (find-match-rules function-name function-variables))
        (let ((cur-index 0) (total-match-rule (length match-rule-indexes)) (cur-result nil) )
        
            (loop 
                (when (>= cur-index total-match-rule) (return ))
            
                (setq cur-result (evaluate-query-for-rule query-input (nth cur-index match-rule-indexes)))
                (if (not (eq cur-result nil))
                    (return-from evaluate-query t)
                )
                (setq cur-index (+ cur-index 1))
            )
        )
        (return-from evaluate-query nil)
    )
)

;; Each transaction from the user is navigated one by one. 
;; If rule is rule-list fact, it is append to fact-list.
;; If it is query, evaluate-query function is called.
;; and the value obtained from here is thrown into each-result list.
;; Each query result thrown to each-result is returned to the process to write to the output file.
(defun evaluate-algorithm (input-list)

    (let ( (cur-index 0) (len-input (length input-list)) (each-result '()) )  
    
        (loop
            (when (>= cur-index len-input) (return ))
                (let ((cur-list (nth cur-index input-list)))

                (if (not (= 2 (length cur-list)))
                    (progn
                        (print "Your input format is wrong. Please make sure that your input is correct")
                        (print "Failed input : ")
                        (print cur-list)
                        (return-from split-input-types nil)
                    )
                )
                (cond 
                    ;; ( ("arms" ("horse" 0)) () )
                    ;; for facts
                    (
                        (eq (nth 1 cur-list) nil)
                        (setq fact-list (append fact-list (list cur-list)))
                    )

                    ;; ( () ("legs" ("horse" 4)) ) 
                    ;; for queries
                    (   
                        ;; EVALUATE QUERY
                        (eq (nth 0 cur-list) nil)
                        (setq query-list (append query-list (list cur-list)))
                        
                        (setq each-result (append each-result (list (evaluate-query (nth 1 cur-list))  ) ) )
                        ;; (return-from evaluate-algorithm (evaluate-query (nth 1 cur-list)))
                    )

                    ;; ( ("legs" ("X" 2)) ( ("mammal" ("X")) ("arms" ("X" 2)) ) )
                    ;; for rules
                    (t 
                        (setq rule-list (append rule-list (list cur-list)))
                    )
                )
            )
            (setq cur-index (+ cur-index 1))
        )
        (return-from evaluate-algorithm each-result)
    )
)

;; writes the result to the output file
(defun write-to-file (result output-filename)

    (let ((cur-index 0) (len-list (length result)))

        (with-open-file (str output-filename
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)

            (loop 
            
                (when (>= cur-index len-list) (return ))

                (if (equal (nth cur-index result) t)
                    (format str "true~%")
                    (format str "false~%")
                )
                (setq cur-index (+ cur-index 1))
            )
        
    )
    )
)

(defun main(input-filename output-filename)

    (let ( (user-input nil) (deneme '()) (string-merged-list nil) (input-list nil) (result nil ) )
        (setq user-input (read-file input-filename))
        
        (setq string-merged-list (merge-string-variables user-input))
        (setq input-list (convert-string-to-list string-merged-list))
        
        (setq result (evaluate-algorithm input-list))
        (write-to-file result output-filename)
    )   
)

(main "input.txt" "output.txt")
